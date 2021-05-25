tar_engine_tar_target <- function(options) {
  tar_engine_target(
    options = options,
    package = "targets",
    factory = "tar_target",
    code = "command",
    prototype = interactive(),
    format_name = "%s"
  )
}

#' @title `knitr` engine for targets
#' @export
#' @family Target Markdown
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` engine to prototype and construct
#'   targets in a literate programming document.
#' @section Target Markdown:
#'   Target Markdown is an interface to prototype and construct
#'   `targets` pipelines entirely in R Markdown. See
#'   <https://books.ropensci.org/targets/markdown.html>
#'   for usage details.
#' @details [tar_engine_target()] is a `knitr` engine to
#'   prototype and construct different kinds of targets.
#'   These engines have two basic behaviors,
#'   depending on whether the R session is interactive.
#'   Interactive sessions are for prototyping,
#'   while non-interactive sessions are for pipeline construction.
#'   In interactive sessions, the engine runs the code in a new
#'   transient environment (in order prevent side effects)
#'   and then assigns the return value
#'   to a variable whose name is the chunk name. This emulates
#'   what happens when a target runs during [tar_make()].
#'   In non-interactive sessions, the engine adds the code
#'   to the existing pipeline without running it. More specifically,
#'   it generates a call to the target factory and
#'   writes that call to a file in `_targets_r/targets/`.
#'   In addition, the engine also writes a `_targets.R`
#'   file that leverages the files in `_targets_r/`
#'   to manage the pipeline.
#' @param options Named list of `knitr` chunk options.
#' @param package Character of length 1, name of the package
#'   that supports the target factory.
#' @param factory Character of length 1, name of the target factory
#'   corresponding to the engine. Defaults to `"tar_target"`
#'   but could be an alternative factory such as `"tar_render"`
#'   (from `tarchetypes`) or `"tar_stan_mcmc"` (from `stantargets`).
#' @param code Character of length 1, name of a formal argument
#'   to the target factory to insert the code from the chunk.
#' @param prototype Logical of length 1, whether to run in interactive
#'   prototype mode (`TRUE`) or non-interactive
#'   pipeline construction mode (`FALSE`).
#' @param format_name Character of length 1 supplied to the
#'   `fmt` argument to `sprintf()` to create the variable
#'   name to assign to the value in prototype/interactive mode.
#'   Should have a single `%s` placeholder to mark where to insert
#'   the chunk label.
tar_engine_target <- function(
  options,
  package = "targets",
  factory = "tar_target",
  code = "command",
  prototype = interactive(),
  format_name = "%s"
) {
  assert_list(options, "chunk options must be a list.")
  assert_nonempty(names(options), "chunk options list must be named.")
  assert_nzchar(names(options), "each chunk option name must not be empty.")
  assert_chr(package, "package must be a character.")
  assert_chr(factory, "factory must be a character.")
  assert_chr(code, "code must be a character.")
  assert_lgl(prototype, "interactive must be logical.")
  assert_chr(format_name, "format_name must be a character.")
  assert_scalar(package, "package must have length 1.")
  assert_scalar(factory, "factory must have length 1.")
  assert_scalar(code, "code must have length 1.")
  assert_scalar(prototype, "interactive must have length 1.")
  assert_scalar(format_name, "format_name must have length 1.")
  if_any(
    prototype,
    tar_engine_target_interactive(
      options = options,
      format_name = format_name
    ),
    tar_engine_target_noninteractive(
      options = options,
      package = package,
      factory = factory,
      code = code
    )
  )
}

tar_engine_target_interactive <- function(options, format_name) {
  assert_package("knitr")
  name <- sprintf(format_name, options$label)
  envir_knitr <- knitr::knit_global()
  envir <- new.env(parent = envir_knitr)
  expr <- parse(text = options$code)
  tidy_eval <- options$tidy_eval %|||% TRUE
  expr <- tar_tidy_eval(expr = expr, envir = envir, tidy_eval = tidy_eval)
  value <- eval(expr, envir = envir)
  assign(x = name, value = value, envir = envir_knitr)
  out <- paste0(
    "Assigned return value to variable ",
    name,
    ". Local variables dropped."
  )
  options$eval <- FALSE
  knitr::engine_output(options = options, code = options$code, out = out)
}

tar_engine_target_noninteractive <- function(
  options,
  package,
  factory,
  code
) {
  write_targets_r()
  # TODO: better name conflict handling
  if (is.logical(options$error)) {
    options$error <- NULL
  }
  fun_name <- paste0(package, "::", factory)
  fun <- get(factory, envir = getNamespace(package))
  # TODO: create better target definition text.
  lines <- c(
    options$label,
    paste(code, "= {", options[[code]]),
    "}"
  )
  for (arg in intersect(names(options), names(formals(fun)))) {
    lines <- c(lines, paste(arg, "=", options[[arg]]))
  }
  # TODO: write to actual file.
  writeLines(lines, "~/Desktop/lines.txt")
  out <- paste0(
    "Wrote ", factory, "() ", options$label, " to file."
  )
  options$eval <- FALSE
  knitr::engine_output(options = options, code = options$code, out = out)
}

write_targets_r <- function() {
  path <- system.file(
    file.path("pipelines", "_targets_r.R"),
    package = "targets",
    mustWork = TRUE
  )
  file.copy(path, path_script(), overwrite = TRUE)
}
