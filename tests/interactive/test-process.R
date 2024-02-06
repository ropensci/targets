tar_test("error running two pipelines on _targets/ at the same time", {
  tar_script(tar_target(x, Sys.sleep(120)))
  process <- tar_make(callr_function = callr::r_bg)
  on.exit(process$kill())
  Sys.sleep(5)
  tar_script(tar_target(x, TRUE))
  for (index in seq_len(2L)) {
    expect_error(
      tar_make(callr_function = NULL),
      class = "tar_condition_validate"
    )
  }
  temp <- tempfile()
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  tar_make(callr_function = NULL, store = temp)
  expect_true(tar_read(x, store = temp))
  process$kill()
  Sys.sleep(5)
  tar_script(tar_target(x, "x"))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x")
  tar_script(tar_target(x, FALSE))
  tar_make(callr_function = NULL)
  expect_false(tar_read(x))
})
