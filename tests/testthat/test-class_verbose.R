tar_test("run verbose reporter", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  for (index in seq_len(2L)) {
    local <- local_init(pipeline, reporter = "verbose")
    expect_message(local$run())
  }
})

tar_test("run verbose reporter with a error and save workspace", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)), error = "workspace")
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
})

tar_test("run timestamp reporter with a warning", {
  pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
  local <- local_init(pipeline, reporter = "verbose")
  suppressWarnings(
    expect_warning(local$run(), class = "tar_condition_run")
  )
})

tar_test("validate verbose reporter", {
  expect_silent(reporter_init("verbose")$validate())
})
