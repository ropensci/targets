tar_test("tar_knitr_engine() construct globals", {
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
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  expect_false(
    file.exists(path_script_r_targets_dir(path_script_default()))
  )
  expect_true(
    file.exists(path_script_r_globals(path_script_default(), "test"))
  )
  tar_make(callr_function = NULL)
  expect_equal(x, "a")
})

tar_test("tar_knitr_engine() construct globals with alternative script path", {
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
    tar_script = script,
    tar_simple = TRUE # should be ignored
  )
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  expect_true(file.exists(script))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_globals(path, "test")))
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
  path <- script
  expect_true(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_globals(path, "test")))
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
  tar_config_set(script = script)
  tar_make(callr_function = NULL)
  expect_equal(x, "a")
})

tar_test("tar_knitr_engine() prototype globals", {
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
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_globals(path, "test")))
  expect_true(exists("x", envir = envir, inherits = FALSE))
  expect_equal(envir$x, "a")
})

tar_test("tar_knitr_engine() construct targets", {
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
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_targets(path, "test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_knitr_engine() construct targets, simple version", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "\"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = FALSE,
    tar_interactive = FALSE,
    tar_simple = TRUE
  )
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_targets(path, "test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(test), "a")
})

tar_test("tar_knitr_engine() construct targets with custom script path", {
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
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  expect_true(file.exists(script))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_globals(path, "test")))
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
  expect_false(file.exists(path_script_r_globals_dir(script)))
  expect_false(file.exists(path_script_r_globals(script, "test")))
  expect_true(file.exists(path_script_r_targets_dir(script)))
  expect_true(file.exists(path_script_r_targets(script, "test")))
  tar_config_set(script = script)
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_knitr_engine() construct targets, alt script path, tar_simple", {
  skip_if_not_installed("knitr")
  script <- "example/script.R"
  options <- list(
    code = "\"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = FALSE,
    tar_interactive = FALSE,
    tar_script = script,
    tar_simple = TRUE
  )
  tar_knitr_engine(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  expect_true(file.exists(script))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_globals(path, "test")))
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
  expect_false(file.exists(path_script_r_globals_dir(script)))
  expect_false(file.exists(path_script_r_globals(script, "test")))
  expect_true(file.exists(path_script_r_targets_dir(script)))
  expect_true(file.exists(path_script_r_targets(script, "test")))
  tar_config_set(script = script)
  tar_make(callr_function = NULL)
  expect_equal(tar_read(test), "a")
})

tar_test("tar_knitr_engine() prototype targets", {
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
  tar_knitr_engine(options)
  expect_equal(envir$x, "a")
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
})

tar_test("tar_knitr_engine() prototype targets, simple version", {
  skip_if_not_installed("knitr")
  options <- list(
    code = "\"a\"",
    echo = FALSE,
    engine = "targets",
    label = "test",
    results = "hide",
    tar_globals = FALSE,
    tar_interactive = TRUE,
    tar_simple = TRUE
  )
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  tar_knitr_engine(options)
  expect_equal(envir$test, "a")
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
})

tar_test("tar_knitr_engine() warning if duplicate chunk labels allowed", {
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
    tar_knitr_engine(options),
    class = "tar_condition_validate"
  )
})

tar_test("knitr_engine_set()", {
  knitr_engine_set()
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
    tar_knitr_engine(options),
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
    tar_knitr_engine(options),
    class = "tar_condition_validate"
  )
})
