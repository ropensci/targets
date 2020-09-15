as_class <- function(x) {
  enclass(list(x), x)
}

enclass <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}
