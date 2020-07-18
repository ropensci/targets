#' @title Include a knitr or R Markdown report in a pipeline.
#' @export
#' @description Register a knitr or R Markdown report as part of a dynamic
#'   target. Relies on tidy evaluation to insert a special expression into
#'   the target's command when the target is defined.
#' @details `tar_knitr()` tells `targets` to look for dependency targets
#'   in the active code chunks of the report. These dependencies
#'   must be mentioned as literal symbols in explicit calls to
#'   [tar_load()] and [tar_read()]. This mechanism not only rerenders
#'   the report automatically when the dependencies change, but also
#'   allows you to run the report by itself (outside the pipeline)
#'   as long as a `_targets/` data store already exists in the current
#'   working directory and contains the data.
#' @section Working directory:
#'   The current working directory (i.e. `getwd()`) must contain the
#'   `_targets/` data store not only when `tar_knitr()` is evaluated,
#'   but also when the actual report is run. The easiest way to
#'   deal with this is just to keep all your R Markdown source files
#'   at the root directory of the project. If you need to use other
#'   directories, consider setting `knit_root_dir = getwd()`
#'   in `rmarkdown::render()` or
#'   `knitr::opts_knit$set(root.dir = your_project_root_directory)`
#'   in an early code chunk of the report itself.
#' @return A language object that represents the dependencies and
#'   return value of a `knitr` source dynamic file.
#' @param path Character of length 1, path to the `knitr` or
#'   R Markdown source file.
#' @examples
#' \dontrun{
#' tar_dir({
#' # Here is a path to an example R Markdown report that depends on
#' # targets data, data2, and analysis.
#' path <- system.file("example_knitr_report.Rmd", package = "targets")
#' # `tar_knitr()` defines a piece of code with the dependencies
#' # of the report as symbols so the code analyzer can detect them.
#' expr <- tar_knitr(path)
#' expr
#' # If you evaluate the expression, you get the path to the report,
#' # which is exactly what we want for dynamic files (`format = "file"`).
#' eval(expr, envir = list(analysis = 1, data = 1, data2 = 1))
#' # In your actual pipeline, write a dynamic file (`format = "file"`) target
#' # to run the report and return the paths to the source and output.
#' # (`!!tar_knitr("report.Rmd")` returns `"report.Rmd"`, and it requires
#' # `tidy_eval` to be `TRUE` in [tar_target()] (default).
#' file.copy(path, "report.Rmd")
#' tar_script({
#'   tar_options()
#'   tar_pipeline(
#'     tar_target(data, create_data()), # You define create_data().
#'     tar_target(analysis, analyze_data(data)), # You define analyze_data().
#'     tar_target(
#'       report, {
#'         rmarkdown::render("report.Rmd")
#'         c(!!tar_knitr("report.Rmd"), "report.html")
#'       },
#'       format = "file"
#'     )
#'   )
#' })
#' # In the graph below,
#' # notice how report depends on data and analysis
#' # because of the calls to tar_load() and tar_read() in the report
#' # and the use of !!tar_knitr() in the target.
#' tar_visnetwork()
#' })
#' }
tar_knitr <- function(path) {
  assert_package("knitr", "tar_knitr() requires the knitr package.")
  assert_scalar(path, "tar_knitr() only takes one file at a time.")
  assert_chr(path, "path argument of tar_knitr() must be a character.")
  assert_path(path, paste("the path", path, "for tar_knitr() does not exist."))
  expr <- knitr_expr(path)
  deps <- rlang::syms(knitr_deps(expr))
  out <- substitute(
    list(deps = deps, path = path)[["path"]],
    env = list(deps = deps, path = path)
  )
  safe_parse(safe_deparse(out))
}

knitr_expr <- function(path) {
  tryCatch(
    parse(text = knitr_code(path)),
    error = function(e) {
      throw_validate(
        "Could not parse knitr report ",
        path,
        " to detect dependencies: ",
        conditionMessage(e)
      )
    }
  )
}

knitr_code <- function(path) {
  handle <- basename(tempfile())
  connection <- textConnection(handle, open = "w", local = TRUE)
  on.exit(close(connection))
  withr::with_options(
    new = list(knitr.purl.inline = TRUE),
    code = knitr::knit(path, output = connection, tangle = TRUE, quiet = TRUE)
  )
  textConnectionValue(connection)
}

knitr_deps <- function(expr) {
  counter <- counter_init()
  walk_expr(expr, counter)
  counter_get_names(counter)
}

walk_expr <- function(expr, counter) {
  if (!length(expr)) {
    return()
  } else if (is.call(expr)) {
    walk_call(expr, counter)
  } else if (typeof(expr) == "closure") {
    walk_expr(formals(expr), counter = counter)
    walk_expr(body(expr), counter = counter)
  } else if (is.pairlist(expr) || is.recursive(expr)) {
    lapply(expr, walk_expr, counter = counter)
  }
}

walk_call <- function(expr, counter) {
  name <- safe_deparse(expr[[1]], backtick = FALSE)
  if (name %in% paste0(c("", "targets::", "targets:::"), "tar_load")) {
    register_load(expr, counter)
  }
  if (name %in% paste0(c("", "targets::", "targets:::"), "tar_read")) {
    register_read(expr, counter)
  }
  lapply(expr, walk_expr, counter = counter)
}

register_load <- function(expr, counter) {
  expr <- match.call(targets::tar_load, as.call(expr))
  names <- all.vars(expr$names, functions = FALSE, unique = TRUE)
  counter_set_names(counter, names)
}

register_read <- function(expr, counter) {
  expr <- match.call(targets::tar_read, as.call(expr))
  names <- all.vars(expr$name, functions = FALSE, unique = TRUE)
  counter_set_names(counter, names)
}
