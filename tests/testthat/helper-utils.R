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

skip_cran <- function() {
  skip_on_cran()
  if (!identical(Sys.getenv("TAR_NOT_CRAN", unset = ""), "true")) {
    skip("running on cran")
  }
}

skip_torch <- function() {
  skip_if_not_installed("torch")
  tryCatch(
    torch::torch_zeros(10),
    error = function(e) skip("torch is not working.")
  )
}

crew_test_sleep <- function() {
  on_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
  if (on_windows || on_cran || on_ci) {
    Sys.sleep(2.25)
  }
}

# TODO: always test and bump required versions when the next
# crew/mirai/nanonext are on CRAN.
skip_if_low_dep_versions <- function() {
  sufficient_versions <- rlang::is_installed(
    pkg = c(
      "crew (>= 0.2.1)",
      "mirai (>= 0.8.7.9012)",
      "nanonext (>= 0.8.3.9007)"
    )
  )
  if (!sufficient_versions) {
    skip("version of crew, mirai, or nanonext is too low")
  }
}
