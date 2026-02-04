tar_test("balanced reporter", {
  skip_cran()
  tar_script({
    list(
      tar_target(
        x,
        {
          Sys.sleep(1)
          seq_len(2L)
        }
      ),
      tar_target(y, Sys.sleep(1), pattern = map(x))
    )
  })
  suppressMessages(
    expect_message(tar_make(callr_function = NULL, reporter = "balanced"))
  )
  tar_script({
    list(
      tar_target(x, stop("intentional error"))
    )
  })
  suppressMessages(
    expect_error(tar_make(callr_function = NULL, reporter = "balanced"))
  )
  suppressMessages(
    expect_message(tar_outdated(callr_function = NULL, reporter = "balanced"))
  )
})
