tar_test("run silent reporter", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  local <- local_init(pipeline, reporter = "silent")
  expect_silent(local$run())
})

tar_test("run silent reporter with a error", {
  pipeline <- pipeline_init(list(target_init("x", quote(stop(123)))))
  local <- local_init(pipeline, reporter = "silent")
  expect_error(local$run())
})

tar_test("misspell reporter name", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  expect_error(
    local_init(pipeline, reporter = "sillllent")$run(),
    class = "tar_condition_validate"
  )
})

tar_test("validate silent reporter", {
  expect_silent(reporter_init("silent")$validate())
})
