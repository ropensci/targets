tar_test("tar_engine() construct globals", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    targets = FALSE
  )
  tar_engine(options, prototype = FALSE)
  expect_true(file.exists(path_script()))
  expect_true(file.exists(path_script_r_globals("test")))
  tar_make(callr_function = NULL)
  expect_equal(x, "a")
})
