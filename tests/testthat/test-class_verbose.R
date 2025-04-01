tar_test("run verbose reporter", {
  pipeline <- pipeline_init(list(target_init("x", quote(0))))
  for (index in seq_len(2L)) {
    local <- local_init(pipeline, reporter = "verbose")
    expect_message(local$run())
  }
})

tar_test("run verbose reporter with a error and save workspace", {
  tar_option_set(workspace_on_error = TRUE)
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(stop(123)))
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  expect_error(expect_message(local$run()), class = "tar_condition_run")
})

tar_test("run verbose reporter workspace upload", {
  skip_on_os("windows")
  skip_on_ci()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(123))
    )
  )
  local <- local_init(pipeline, reporter = "verbose")
  local$run()
  expect_message(
    local$scheduler$reporter$report_workspace_upload(pipeline$targets$x)
  )
})

tar_test("run verbose reporter with a warning", {
  pipeline <- pipeline_init(list(target_init("x", quote(warning(123)))))
  local <- local_init(pipeline, reporter = "verbose")
  suppressWarnings(
    expect_warning(local$run(), class = "tar_condition_run")
  )
})

tar_test("verbose reporter retry message", {
  skip_on_os("windows")
  pipeline <- pipeline_init(list(target_init("x", quote(TRUE))))
  local <- local_init(pipeline, reporter = "verbose")
  local$run()
  expect_message(
    local$scheduler$reporter$report_retry(
      target = local$pipeline$targets$x,
      progress = local$scheduler$progress
    )
  )
})

tar_test("report_pattern()", {
  x <- verbose_new()
  expect_message(x$report_pattern(tar_target(x, TRUE, pattern = map(y))))
})

tar_test("validate verbose reporter", {
  expect_silent(reporter_init("verbose")$validate())
})
