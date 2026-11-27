#' @title Locally set target options.
#' @export
#' @family configuration
#' @description Locally set target options for the duration
#'   of an expression, without permanently modifying the global state.
#' @return `NULL` (invisibly).
#' @param expression An R expression to run with the local option.
#' @param ... Named arguments to [tar_option_set()] to temporarily set
#'   for the duration of `expression`.
#' @param envir_with Environment to evaluate `expression`.
#' @examples
#'   tar_option_with(
#'     tar_target(data, get_data()),
#'     packages = "dplyr",
#'     cue = tar_cue(mode = "never")
#'   )
tar_option_with <- function(expression, ..., envir_with = parent.frame()) {
  expression <- substitute(expression)
  options <- list(...)
  force(envir_with)
  option_names <- names(options)
  tar_assert_in(option_names, names(formals(tar_option_set)))
  old <- lapply(option_names, function(name) tar_options[[name]])
  names(old) <- option_names
  on.exit(tar_option_restore(old))
  do.call(tar_option_set, options)
  eval(expression, envir = envir_with)
}

tar_option_restore <- function(old) {
  for (name in intersect(names(old), names(tar_options))) {
    tar_options[[name]] <- old[[name]]
  }
}
