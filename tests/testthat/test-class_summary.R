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

tar_test("summary reporter retry message", {
  skip_on_os("windows")
  pipeline <- pipeline_init(
    list(target_init("x", quote(TRUE)))
  )
  local <- local_init(pipeline, reporter = "summary")
  expect_message(local$run())
  expect_message(
    local$scheduler$reporter$report_retry(progress = local$scheduler$progress)
  )
})

tar_test("validate summary reporter", {
  expect_silent(reporter_init("summary")$validate())
})
