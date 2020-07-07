enclass <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}
