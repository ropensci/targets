tar_test("tar_workspaces()", {
  expect_equal(tar_workspaces(), character(0))
  tar_script({
    tar_option_set(workspaces = c("x", "y"))
    list(tar_target(x, "value"), tar_target(y, "value"))
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_workspaces(), sort(c("x", "y")))
  expect_equal(tar_workspaces(contains("x")), "x")
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    {
      tar_option_set(workspace_on_error = TRUE)
      list(tar_target(x, "value"), tar_target(y, stop(x)))
    },
    script = "example/script.R"
  )
  expect_false(file.exists("example/store/workspaces/y"))
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  expect_true(file.exists("example/store/workspaces/y"))
  out <- tar_workspaces(store = "example/store")
  expect_true("y" %in% out)
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
