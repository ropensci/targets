map <- function(x, f, ...) {
  lapply(X = x, FUN = as_function(f), ...)
}

map_chr <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = character(1), ...)
}

map_dbl <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = numeric(1), ...)
}

map_int <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = integer(1), ...)
}

map_lgl <- function(x, f, ...) {
  vapply(X = x, FUN = as_function(f), FUN.VALUE = logical(1), ...)
}

map_rows <- function(x, f, ...) {
  apply(X = x, MARGIN = 1L, FUN = as_function(f), ...)
}

fltr <- function(x, f, ...) {
  index <- map_lgl(x, f, ...)
  x[index]
}
