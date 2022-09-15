tar_test("tar_engine_knitr() noninteractive globals", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  tar_engine_knitr(options)
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

tar_test("same with eval = FALSE", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$eval <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  tar_engine_knitr(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  expect_false(
    file.exists(path_script_r_targets_dir(path_script_default()))
  )
  expect_false(
    file.exists(path_script_r_globals(path_script_default(), "test"))
  )
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_validate"
  )
})

tar_test("tar_engine_knitr() noninteractive globals with tar_name", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$tar_name <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  tar_engine_knitr(options)
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

tar_test("tar_engine_knitr() noninteractive globals, alt script path", {
  skip_cran()
  skip_if_not_installed("knitr")
  script <- "example/script.R"
  options <- knitr::opts_chunk$get()
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  options$tar_script <- script
  options$tar_simple <- TRUE # should be ignored
  tar_engine_knitr(options)
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

tar_test("tar_engine_knitr() interactive globals", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- TRUE
  envir <- new.env(parent = baseenv())
  old <- tar_option_get("envir")
  on.exit(tar_option_set(envir = old))
  tar_option_set(envir = envir)
  expect_false(exists("x", envir = envir, inherits = FALSE))
  tar_engine_knitr(options)
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_targets_dir(path)))
  expect_false(file.exists(path_script_r_globals(path, "test")))
  expect_true(exists("x", envir = envir, inherits = FALSE))
  expect_equal(envir$x, "a")
})

tar_test("tar_engine_knitr() noninteractive targets", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "tar_target(x, \"a\")"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- FALSE
  tar_engine_knitr(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_targets(path, "test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_engine_knitr() noninteractive targets with tar_name", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "tar_target(x, \"a\")"
  options$echo <- FALSE
  options$engine <- "targets"
  options$tar_name <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- FALSE
  tar_engine_knitr(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_targets(path, "test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "a")
})

tar_test("tar_engine_knitr() noninteractive targets, simple version", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "\"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- FALSE
  options$tar_simple <- TRUE
  tar_engine_knitr(options)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_true(file.exists(path_script_r_targets(path, "test")))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(test), "a")
})

tar_test("tar_engine_knitr() noninteractive targets with custom script path", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  script <- "example/script.R"
  options$code <- "tar_target(x, \"a\")"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- FALSE
  options$tar_script <- script
  tar_engine_knitr(options)
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

tar_test("tar_engine_knitr() non-int targets, alt script path, tar_simple", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  script <- "example/script.R"
  options$code <- "\"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- FALSE
  options$tar_script <- script
  options$tar_simple <- TRUE
  tar_engine_knitr(options)
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

tar_test("tar_engine_knitr() interactive targets", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "targets::tar_target(x, \"a\")"
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- TRUE
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  tar_engine_knitr(options)
  expect_equal(envir$x, "a")
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
})

tar_test("tar_engine_knitr() interactive targets, simple version", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  options$code <- "\"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- FALSE
  options$tar_interactive <- TRUE
  options$tar_simple <- TRUE
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  tar_engine_knitr(options)
  expect_equal(envir$test, "a")
  expect_false(file.exists(path_store_default()))
  expect_false(file.exists(path_script_default()))
  path <- path_script_default()
  expect_false(file.exists(path_script_r_globals_dir(path)))
  expect_false(file.exists(path_script_r_targets(path, "test")))
})

tar_test("tar_engine_knitr() warning if duplicate chunk labels allowed", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  old_envvar <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "true")
  old_option <- getOption("knitr.duplicate.label")
  on.exit({
    options(knitr.duplicate.label = old_option)
    Sys.setenv(TAR_WARN = old_envvar)
  })
  options(knitr.duplicate.label = "allow")
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  expect_warning(
    tar_engine_knitr(options),
    class = "tar_condition_validate"
  )
})

tar_test("engine_knitr_set()", {
  skip_cran()
  skip_if_not_installed("knitr")
  engine_knitr_set()
  engine_names <- names(knitr::knit_engines$get())
  expect_true("targets" %in% engine_names)
})

tar_test("deprecated targets option", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  option <- getOption("knitr.duplicate.label")
  on.exit(options(knitr.duplicate.label = option))
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "test"
  options$results <- "hide"
  options$targets <- TRUE
  options$tar_interactive <- FALSE
  expect_warning(
    tar_engine_knitr(options),
    class = "tar_condition_deprecate"
  )
})

tar_test("unnamed chunk label", {
  skip_cran()
  skip_if_not_installed("knitr")
  options <- knitr::opts_chunk$get()
  old <- Sys.getenv("TAR_WARN")
  on.exit(Sys.setenv(TAR_WARN = old))
  Sys.setenv(TAR_WARN = "true")
  options$code <- "x <- \"a\""
  options$echo <- FALSE
  options$engine <- "targets"
  options$label <- "unnamed-chunk-1"
  options$results <- "hide"
  options$tar_globals <- TRUE
  options$tar_interactive <- FALSE
  expect_warning(
    tar_engine_knitr(options),
    class = "tar_condition_validate"
  )
})

tar_test("conflicting target scripts", {
  skip_cran()
  skip_if_not_installed("knitr")
  write_targets_r("_targets.R")
  expect_silent(write_targets_r("_targets.R"))
  lines1 <- readLines("_targets.R")
  lines2 <- lines1[-length(lines1)]
  writeLines(lines2, "_targets.R")
  expect_equal(readLines("_targets.R"), lines2)
  expect_silent(write_targets_r("_targets.R"))
  expect_equal(readLines("_targets.R"), lines1)
  writeLines(lines1[-1], "_targets.R")
  expect_error(write_targets_r("_targets.R"), class = "tar_condition_validate")
})
