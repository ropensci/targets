`%||%` <- function(x, y) {
  if (length(x) <= 0L) {
    y
  } else {
    x
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%||NA%` <- function(x, y) {
  if (anyNA(x)) {
    y
  } else {
    x
  }
}

#' @title Non-vectorized if/else.
#' @export
#' @keywords internal
#' @description Internal to `targets`. Not a user-side function.
#'   Do not invoke directly. Only exported for the purpose
#'   of using `callr` functions.
#' @return `x` if `any(condition)` evaluates to `TRUE`,
#'   otherwise `y`.
#' @param condition A value or expression that evaluates to
#'   a logical of length 1 when invoked through `any()`.
#' @param `x` Value to return if `any(condition)`
#'   evaluates to `TRUE`.
#' @param `y` Value to return if `any(condition)`
#'   does not evaluate to `TRUE`.
#' @examples
#' if_any(TRUE, 1, 2)
#' if_any(FALSE, 1, 2)
if_any <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}
