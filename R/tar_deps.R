#' @title Code dependencies
#' @export
#' @family inspect
#' @seealso [tar_branches()], [tar_network()]
#' @description List the dependencies of a function or expression.
#'   [tar_deps()] expects the `expr` argument to be an unevaluated
#'   expression,
#'   whereas [tar_deps_raw()] expects `expr` to be an evaluated
#'   expression object. Functions can be passed normally in either case.
#' @details `targets` detects the dependencies of commands using
#'   static code analysis. Use `tar_deps()` to run the
#'   code analysis and see the dependencies for yourself.
#' @return Character vector of the dependencies of a function or expression.
#' @param expr An R expression or function.
#'   [tar_deps()] expects the `expr` argument to be an unevaluated
#'   expression,
#'   whereas [tar_deps_raw()] expects `expr` to be an evaluated
#'   expression object.
#'   Functions can be passed normally in either case.
#' @examples
#' tar_deps(x <- y + z)
#' tar_deps(quote(x <- y + z))
#' tar_deps({
#'   x <- 1
#'   x + a
#' })
#' tar_deps(function(a = b) map_dfr(data, ~do_row(.x)))
#' tar_deps_raw(function(a = b) map_dfr(data, ~do_row(.x)))
tar_deps <- function(expr) {
  expr <- substitute(expr)
  tar_deps_raw(expr)
}
