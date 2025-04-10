test_that("terse reporter", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, seq_len(4L)),
      tar_target(y, x, pattern = map(x))
    )
  })
  suppressMessages(
    expect_message(tar_make(callr_function = NULL, reporter = "terse"))
  )
  tar_script({
    list(
      tar_target(x, stop("intentional error"))
    )
  })
  suppressMessages(
    expect_error(tar_make(callr_function = NULL, reporter = "terse"))
  )
  suppressMessages(
    expect_message(tar_outdated(callr_function = NULL, reporter = "terse"))
  )
})
