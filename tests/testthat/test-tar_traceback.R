tar_test("tar_traceback()", {
  tar_script({
    tar_option_set(workspace_on_error = TRUE)
    list(tar_target(y, stop("3c47b24bd4a7ad8e5ce70f05eefe7c9c")))
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  out <- tar_traceback(y)
  expect_true(any(grepl("3c47b24bd4a7ad8e5ce70f05eefe7c9c", out)))
})

tar_test("tar_traceback()", {
  tar_script({
    tar_option_set(workspaces = "z")
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  out <- tar_traceback(z)
  expect_equal(out, character(0))
})

tar_test("tar_traceback() deprecated arguments", {
  tar_script({
    tar_option_set(workspaces = "z")
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  expect_warning(
    tar_traceback(z, envir = emptyenv()),
    class = "tar_condition_deprecate"
  )
  expect_warning(
    tar_traceback(z, characters = 2L),
    class = "tar_condition_deprecate"
  )
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    {
      tar_option_set(workspace_on_error = TRUE)
      list(tar_target(y, stop("3c47b24bd4a7ad8e5ce70f05eefe7c9c")))
    },
    script = "example/script.R"
  )
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  out <- tar_traceback(y, store = "example/store")
  expect_true(is.character(out))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/workspaces/y"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
