tar_test("run summary reporter", {
  for (index in seq_len(2L)) {
    pipeline <- pipeline_init(list(target_init("x", quote(0))))
    local <- local_init(pipeline, reporter = "summary")
    expect_message(local$run())
  }
})

tar_test("run summary reporter with a error", {
  pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
  local <- local_init(pipeline, reporter = "summary")
  expect_error(expect_message(local$run()))
})

tar_test("run summary reporter with a cancellation", {
  pipeline <- pipeline_init(
    list(target_init("x", quote(targets::tar_cancel())))
  )
  local <- local_init(pipeline, reporter = "summary")
  expect_message(local$run())
})

tar_test("validate summary reporter", {
  expect_silent(reporter_init("summary")$validate())
})
