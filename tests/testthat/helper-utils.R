expect_equiv <- function(object, expected, ...) {
  attributes(object) <- NULL
  attributes(expected) <- NULL
  expect_equal(object, expected, ...)
}

tmpenv <- function(...) {
  list2env(list(...), parent = emptyenv())
}

require_clustermq <- function() {
  skip_if_not_installed("clustermq", minimum_version = "0.9.2")
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

crew_test_controller <- function() {
  if (isTRUE(as.logical(Sys.getenv("R_COVR", "false")))) {
    controller <- crew::crew_controller_sequential()
  } else {
    controller <- crew::crew_controller_local(
      host = "127.0.0.1",
      tasks_max = 1L
    )
  }
}

crew_test_sleep <- function() {
  on_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
  if (on_windows || on_cran || on_ci) {
    Sys.sleep(2.25)
  }
}

# Pretend to run outside R CMD check so the trust gate for
# custom storage code is active and can be tested.
# Returns a function that restores the original state.
mock_outside_check <- function() {
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
  function() {
    if (!is.na(old)) {
      Sys.setenv(`_R_CHECK_PACKAGE_NAME_` = old)
    }
  }
}
