#' @title `knitr` engine for Target Markdown
#' @export
#' @family Target Markdown
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` engine to prototype and construct
#'   targets in a literate programming document.
#' @details Target Markdown is an interface to prototype and construct
#'   `targets` pipelines entirely in R Markdown. See
#'   <https://books.ropensci.org/targets/markdown.html>
#'   for usage details.
#' @param options Named list of `knitr` chunk options.
#' @param interactive Logical, whether to run in interactive mode.
tar_engine <- function(options, interactive = interactive()) {
  assert_list(options, "knitr chunk options must be a list.")
  warn_duplicate_labels()
  if_any(
    identical(options$targets, FALSE),
    tar_engine_global(options, interactive),
    tar_engine_target(options, interactive)
  )
}

tar_engine_global <- function(options, interactive) {
  if_any(
    interactive,
    tar_engine_global_interactive(options),
    tar_engine_global_noninteractive(options)
  )
}

tar_engine_target <- function(options, interactive) {
  if_any(
    interactive,
    tar_engine_target_interactive(options),
    tar_engine_target_noninteractive(options)
  )
}

tar_engine_global_interactive <- function(options) {
  eval(parse(text = options$code), envir = tar_option_get("envir"))
}

tar_engine_global_noninteractive <- function(options) {
  
}

tar_engine_target_interactive <- function(options) {
  
}

tar_engine_target_noninteractive <- function(options) {
  
}

warn_duplicate_labels <- function() {
  if (identical(getOption("knitr.duplicate.label", "allow"))) {
    warn_validate(
      "knitr.duplicate.label is set to \"allow\". Duplicate labels ",
      "interfere with the proper execution of Target Markdown. ",
      "Please set knitr.duplicate.label to a value other than \"allow\" ",
      "to prohibit duplicate knitr chunk labels."
    )
  }
}










#   if_any(
#     prototype,
#     tar_engine_target_interactive(
#       options = options,
#       format_name = format_name
#     ),
#     tar_engine_target_noninteractive(
#       options = options,
#       package = package,
#       factory = factory,
#       code = code
#     )
#   )
# }
# 
# 
# 
# tar_engine_target_interactive <- function(options, format_name) {
#   assert_package("knitr")
#   name <- sprintf(format_name, options$label)
#   envir_knitr <- knitr::knit_global()
#   envir <- new.env(parent = envir_knitr)
#   expr <- parse(text = options$code)
#   tidy_eval <- options$tidy_eval %|||% TRUE
#   expr <- tar_tidy_eval(expr = expr, envir = envir, tidy_eval = tidy_eval)
#   value <- eval(expr, envir = envir)
#   assign(x = name, value = value, envir = envir_knitr)
#   out <- paste0(
#     "Assigned return value to variable ",
#     name,
#     ". Local variables dropped."
#   )
#   options$eval <- FALSE
#   knitr::engine_output(options = options, code = options$code, out = out)
# }
# 
# tar_engine_target_noninteractive <- function(
#   options,
#   package,
#   factory,
#   code
# ) {
#   write_targets_r()
#   # TODO: better name conflict handling
#   if (is.logical(options$error)) {
#     options$error <- NULL
#   }
#   fun_name <- paste0(package, "::", factory)
#   fun <- get(factory, envir = getNamespace(package))
#   # TODO: create better target definition text.
#   lines <- c(
#     options$label,
#     paste(code, "= {", options[[code]]),
#     "}"
#   )
#   for (arg in intersect(names(options), names(formals(fun)))) {
#     lines <- c(lines, paste(arg, "=", options[[arg]]))
#   }
#   # TODO: write to actual file.
#   writeLines(lines, "~/Desktop/lines.txt")
#   out <- paste0(
#     "Wrote ", factory, "() ", options$label, " to file."
#   )
#   options$eval <- FALSE
#   knitr::engine_output(options = options, code = options$code, out = out)
# }
# 
# write_targets_r <- function() {
#   path <- system.file(
#     file.path("pipelines", "_targets_r.R"),
#     package = "targets",
#     mustWork = TRUE
#   )
#   file.copy(path, path_script(), overwrite = TRUE)
# }
