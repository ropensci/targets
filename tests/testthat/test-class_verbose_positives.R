tar_test("run verbose_positives reporter", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  for (index in seq_len(2L)) {
    local <- local_init(pipeline, reporter = "verbose_positives")
    expect_message(local$run())
  }
})

tar_test("run verbose_positives reporter with a error and save workspace", {
  tar_option_set(workspace_on_error = TRUE)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)))
    )
  )
  local <- local_init(pipeline, reporter = "verbose_positives")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
})

tar_test("run verbose_positives reporter with a warning", {
  pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
  local <- local_init(pipeline, reporter = "verbose_positives")
  suppressWarnings(
    expect_warning(local$run(), class = "tar_condition_run")
  )
})

tar_test("verbose_positives reporter skipped", {
  tar_script(tar_target(x, 1))
  expect_message(
    tar_make(callr_function = NULL, reporter = "verbose_positives")
  )
  expect_message(
    tar_make(callr_function = NULL, reporter = "verbose_positives")
  )
})

tar_test("validate verbose_positives reporter", {
  expect_silent(reporter_init("verbose_positives")$validate())
})
