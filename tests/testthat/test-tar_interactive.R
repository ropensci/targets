tar_test("tar_interactive() interactive globals", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- tar_interactive(1L + 1L)"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- TRUE
  tar_engine_knitr(options)
  expect_equal(tar_option_get("envir")$x, 2L)
})

tar_test("tar_interactive() interactive targets", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "tar_interactive(1L + 2L)"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_simple <- TRUE
  options$tar_globals <- FALSE
  options$tar_interactive <- TRUE
  tar_engine_knitr(options)
  expect_equal(tar_option_get("envir")$test, 3L)
})

tar_test("tar_interactive() noninteractive", {
  skip_if_not_installed("knitr")
  tar_runtime$interactive <- FALSE
  on.exit(tar_runtime$interactive <- NULL)
  expect_null(tar_interactive(1L + 1L))
})
