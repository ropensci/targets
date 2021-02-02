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
