tar_test("workspaces are not saved if error = 'stop'", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)), error = "stop")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_false(file.exists(store_path_workspace("x")))
})

tar_test("workspaces are not saved if error = 'continue'", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)), error = "continue")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_message(local$run(), class = "condition_run")
  expect_false(file.exists(store_path_workspace("x")))
})

tar_test("workspaces are saved if error = 'save'", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)), error = "save")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "condition_run")
  expect_true(file.exists(store_path_workspace("x")))
})
