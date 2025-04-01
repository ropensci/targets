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
  tar_assert_in(names(options), names(formals(tar_option_set)))
  old <- lapply(names(options), tar_option_get)
  names(old) <- names(options)
  on.exit(do.call(tar_option_set, old))
  do.call(tar_option_set, options)
  eval(expression, envir = envir_with)
}
