as_class_first <- function(x) {
  enclass(list(x), x[1])
}

enclass <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}
