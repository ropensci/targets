expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

tmpenv <- function(...) {
  list2env(list(...), parent = emptyenv())
}
