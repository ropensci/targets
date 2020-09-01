tar_test("tar_undebug()", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(1)),
      target_init("x", quote(stop(y)), error = "save")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_true(file.exists(store_dir_workspaces()))
  tar_undebug()
  expect_false(file.exists(store_dir_workspaces()))
  expect_silent(tar_undebug())
})
