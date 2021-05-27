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

tar_test("tar_engine() prototype globals", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    targets = FALSE
  )
  envir <- new.env(parent = baseenv())
  old <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old))
  tar_option_set(envir = envir)
  expect_false(exists("x", envir = envir, inherits = FALSE))
  tar_engine(options, prototype = TRUE)
  expect_false(file.exists(path_script()))
  expect_false(file.exists(path_script_r_globals("test")))
  expect_true(exists("x", envir = envir, inherits = FALSE))
  expect_equal(envir$x, "a")
})
