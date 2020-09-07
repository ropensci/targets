`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%|||%` <- function(x, y) {
  if (!length(x)) {
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

#' @title Internal infrastructure function.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Only use for developing external HPC backend packages.
#' @param condition Logical, whether to choose x or y.
#' @param x Object to choose of `condition` is `TRUE`.
#' @param y Object to choose of `condition` is `FALSE`.
trn <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}
