tar_test("tar_engine() construct globals", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = TRUE,
    tar_interactive = FALSE
  )
  tar_engine(options)
  expect_false(file.exists(path_store()))
  expect_true(file.exists(path_script()))
  expect_false(file.exists(path_script_r_targets_dir(path_script())))
  expect_true(file.exists(path_script_r_globals("test", path_script())))
  tar_make(callr_function = NULL)
  expect_equal(x, "a")
})

tar_test("tar_engine() construct globals with alternative script path", {
  skip_if_not_installed("knitr")
  script <- "example/script.R"
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = TRUE,
    tar_interactive = FALSE,
    tar_script = script
  )
  tar_engine(options)
  expect_false(file.exists(path_store()))
  expect_false(file.exists(path_script()))
  expect_true(file.exists(script))
  expect_false(file.exists(path_script_r_globals_dir(path_script())))
  expect_false(file.exists(path_script_r_globals("test", path_script())))
  expect_true(file.exists(path_script_r_globals_dir(script)))
  expect_true(file.exists(path_script_r_globals("test", script)))
  tar_config_set(script = script)
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
    tar_globals = TRUE,
    tar_interactive = TRUE
  )
  envir <- new.env(parent = baseenv())
  old <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old))
  tar_option_set(envir = envir)
  expect_false(exists("x", envir = envir, inherits = FALSE))
  tar_engine(options)
  expect_false(file.exists(path_store()))
  expect_false(file.exists(path_script()))
  expect_false(file.exists(path_script_r_targets_dir(path_script())))
  expect_false(file.exists(path_script_r_globals("test", path_script())))
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
    tar_globals = FALSE,
    tar_interactive = FALSE
  )
  tar_engine(options)
  expect_false(file.exists(path_store()))
  expect_true(file.exists(path_script()))
  expect_false(file.exists(path_script_r_globals_dir(path_script())))
  expect_true(file.exists(path_script_r_targets("test", path_script())))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_engine() construct targets with an alternative script path", {
  skip_if_not_installed("knitr")
  script <- "example/script.R"
  options <- list(
    code = "tar_target(x, \"a\")",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = FALSE,
    tar_interactive = FALSE,
    tar_script = script
  )
  tar_engine(options)
  expect_false(file.exists(path_store()))
  expect_false(file.exists(path_script()))
  expect_true(file.exists(script))
  expect_false(file.exists(path_script_r_targets_dir(path_script())))
  expect_false(file.exists(path_script_r_targets("test", path_script())))
  expect_true(file.exists(path_script_r_targets_dir(script)))
  expect_true(file.exists(path_script_r_targets("test", script)))
  tar_config_set(script = script)
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
    tar_globals = FALSE,
    tar_interactive = TRUE
  )
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  tar_engine(options)
  expect_equal(envir$x, "a")
  expect_false(file.exists(path_store()))
  expect_false(file.exists(path_script()))
  expect_false(file.exists(path_script_r_globals_dir(path_script())))
  expect_false(file.exists(path_script_r_targets("test", path_script())))
})

tar_test("tar_engine() warning if duplicate chunk labels allowed", {
  skip_if_not_installed("knitr")
  option <- getOption("knitr.duplicate.label")
  on.exit(options(knitr.duplicate.label = option))
  options(knitr.duplicate.label = "allow")
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = TRUE,
    tar_interactive = FALSE
  )
  expect_warning(
    tar_engine(options),
    class = "tar_condition_validate"
  )
})

tar_test("tar_engine_set()", {
  tar_engine_set()
  engine_names <- names(knitr::knit_engines$get())
  expect_true("targets" %in% engine_names)
})

tar_test("deprecated targets option", {
  skip_if_not_installed("knitr")
  option <- getOption("knitr.duplicate.label")
  on.exit(options(knitr.duplicate.label = option))
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    targets = TRUE,
    tar_interactive = FALSE
  )
  expect_warning(
    tar_engine(options),
    class = "tar_condition_deprecate"
  )
})

tar_test("unnamed chunk label", {
  skip_if_not_installed("knitr")
  old <- Sys.getenv("TAR_WARN")
  on.exit(Sys.setenv(TAR_WARN = old))
  Sys.setenv(TAR_WARN = "true")
  options <- list(
    code = "x <- \"a\"",
    echo = FALSE,
    engine = "targets",
    label = "unnamed-chunk-1",
    results = "hide",
    tar_globals = TRUE,
    tar_interactive = FALSE
  )
  expect_warning(
    tar_engine(options),
    class = "tar_condition_validate"
  )
})
