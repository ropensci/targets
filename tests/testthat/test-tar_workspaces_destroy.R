tar_test("tar_workspaces_destroy()", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "workspace")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_true(file.exists(path_workspaces_dir()))
  tar_workspaces_destroy()
  expect_false(file.exists(path_workspaces_dir()))
  expect_silent(tar_workspaces_destroy())
})
