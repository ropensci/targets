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
  expect_false(file.exists(path_script_r_targets_dir()))
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
  expect_false(file.exists(path_script_r_targets_dir()))
  expect_false(file.exists(path_script_r_globals("test")))
  expect_true(exists("x", envir = envir, inherits = FALSE))
  expect_equal(envir$x, "a")
})

tar_test("tar_engine() construct targets", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "tar_target(x, \"a\")",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    targets = TRUE
  )
  tar_engine(options, prototype = FALSE)
  expect_true(file.exists(path_script()))
  expect_false(file.exists(path_script_r_globals_dir()))
  expect_true(file.exists(path_script_r_targets("test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_engine() prototype targets", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "targets::tar_target(x, \"a\")",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    targets = TRUE
  )
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  tar_engine(options, prototype = TRUE)
  expect_equal(envir$x, "a")
  expect_false(file.exists(path_store()))
  expect_false(file.exists(path_script()))
  expect_false(file.exists(path_script_r_globals_dir()))
  expect_false(file.exists(path_script_r_targets("test")))
})
