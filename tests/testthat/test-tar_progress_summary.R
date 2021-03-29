tar_test("progress does not exist", {
  expect_error(tar_progress_summary(), class = "tar_condition_validate")
})

tar_test("default progress", {
  skip_on_cran()
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary()
  expect_equal(
    colnames(out),
    c("started", "built", "errored", "canceled", "since")
  )
  expect_equal(out$started, 0L)
  expect_equal(out$built, 4L)
  expect_equal(out$errored, 2L)
  expect_equal(out$canceled, 5L)
  expect_true(is.character(out$since))
})

tar_test("progress with tidyselect fields", {
  skip_on_cran()
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary(all_of("time"))
  expect_equal(colnames(out), c("time"))
  expect_true(is.character(out$time))
})

tar_test("tar_progress_summary_gt()", {
  skip_on_cran()
  skip_if_not_installed("gt")
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary_gt()
  expect_true(inherits(out, "gt_tbl"))
})
