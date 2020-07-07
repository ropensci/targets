tar_test("run timestamp reporter", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  for (index in seq_len(2L)) {
    local <- algorithm_init("local", pipeline, reporter = "timestamp")
    expect_message(local$run())
  }
})

tar_test("run timestamp reporter with a error", {
  pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
  local <- algorithm_init("local", pipeline, reporter = "timestamp")
  expect_error(expect_message(local$run()))
})

tar_test("run timestamp reporter with a cancellation", {
  pipeline <- pipeline_init(
    list(target_init("x", quote(targets::tar_cancel())))
  )
  local <- algorithm_init("local", pipeline, reporter = "timestamp")
  expect_message(local$run())
})

tar_test("validate timestamp reporter", {
  expect_silent(reporter_init("timestamp")$validate())
})
