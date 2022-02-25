expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

tmpenv <- function(...) {
  list2env(list(...), parent = emptyenv())
}

require_clustermq <- function() {
  skip_if_not_installed("clustermq")
  skip_if(Sys.getenv("TAR_SKIP_CLUSTERMQ") != "", "skipping clustermq tests")
}
