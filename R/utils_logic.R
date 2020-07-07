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

trn <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}
