#' @title Code dependencies
#' @export
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

#' @title Code dependencies (raw version)
#' @export
#' @description Same as [tar_deps()] except `expr` must already be an
#'   unquoted function or expression object.
#' @return Character vector of the dependencies of a function or expression.
#' @param expr An R expression object or function.
#' @examples
#' tar_deps_raw(quote(x <- y + z))
#' tar_deps_raw(
#'   quote({
#'     x <- 1
#'     x + a
#'   })
#' )
#' tar_deps_raw(function(a = b) map_dfr(data, ~do_row(.x)))
tar_deps_raw <- function(expr) {
  if (!is.function(expr)) {
    expr <- embody_expr(expr)
  }
  deps_function(expr)
}
