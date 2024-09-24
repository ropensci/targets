#' @rdname tar_deps
#' @export
tar_deps_raw <- function(expr) {
  if (!is.function(expr)) {
    expr <- embody_expr(expr)
  }
  deps_function(expr)
}
