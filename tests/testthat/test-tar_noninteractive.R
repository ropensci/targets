tar_test("tar_noninteractive() interactive globals", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- tar_noninteractive(1L + 1L)"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- TRUE
  tar_engine_knitr(options)
  envir <- tar_option_get("envir")
  expect_null(envir$x)
  envir$x <- NULL
})

tar_test("tar_noninteractive() interactive targets", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "tar_noninteractive(1L + 2L)"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_simple <- TRUE
  options$tar_globals <- FALSE
  options$tar_interactive <- TRUE
  tar_engine_knitr(options)
  envir <- tar_option_get("envir")
  expect_null(envir$test)
  envir$test <- NULL
})

tar_test("tar_noninteractive() noninteractive", {
  skip_if_not_installed("knitr")
  tar_runtime$set_interactive(FALSE)
  on.exit(tar_runtime$unset_interactive())
  expect_equal(tar_noninteractive(1L + 1L), 2L)
})
