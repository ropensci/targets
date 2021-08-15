tar_test("tar_toggle() interactive globals", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- tar_toggle(1L + 1L, NULL)"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- TRUE
  tar_engine_knitr(options)
  expect_equal(tar_option_get("envir")$x, 2L)
})

tar_test("tar_toggle() interactive targets", {
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "tar_toggle(1L + 2L)"
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

tar_test("tar_toggle() noninteractive", {
  skip_if_not_installed("knitr")
  tar_runtime$set_interactive(FALSE)
  on.exit(tar_runtime$unset_interactive())
  expect_equal(tar_toggle(1L, 2L), 2L)
})
