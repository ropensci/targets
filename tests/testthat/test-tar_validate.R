tar_test("tar_validate() on a good pipeline", {
  skip_cran()
  tar_script(list(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("tar_validate() works inside callr", {
  skip_cran()
  tar_script(list(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_arguments = list(show = FALSE)))
})

tar_test("tar_validate() warns about name conflicts", {
  skip_cran()
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "true")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    envir <- new.env(parent = globalenv())
    envir$x <- 1
    tar_option_set(envir = envir)
    list(tar_target(x, 1 + 1))
  })
  # The warning happens twice for tar_validate(). # nolint
  suppressWarnings(
    expect_warning(
      tar_validate(callr_function = NULL),
      class = "tar_condition_validate"
    )
  )
})

tar_test("suppress warning about name conflicts", {
  skip_cran()
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "false")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    envir <- new.env(parent = globalenv())
    envir$x <- 1
    tar_option_set(envir = envir)
    list(tar_target(x, 1 + 1))
  })
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  tar_validate(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  tar_validate(
    script = "example/script.R",
    store = "example/store"
  )
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
