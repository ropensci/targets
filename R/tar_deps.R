#' @title Code dependencies
#' @export
#' @family inspect
#' @description List the dependencies of a function or expression.
#' @details `targets` detects the dependencies of commands using
#'   static code analysis. Use `tar_deps()` to run the
#'   code analysis and see the dependencies for yourself.
#' @return Character vector of the dependencies of a function or expression.
#' @param expr A quoted R expression or function.
#' @examples
#' tar_deps(x <- y + z)
#' tar_deps({
#'   x <- 1
#'   x + a
#' })
#' tar_deps(function(a = b) map_dfr(data, ~do_row(.x)))
tar_deps <- function(expr) {
  expr <- substitute(expr)
  tar_deps_raw(expr)
}
