tar_test("tar_poll() with default columns", {
  tar_script({
    list(
      tar_target(x, seq_len(100)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  })
  tar_poll()
  # Open a separate R session and run the pipeline there.
  # Watch the output of tar_poll()
})

tar_test("tar_poll() with non-default columns", {
  tar_script({
    list(
      tar_target(x, seq_len(100)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  })
  px <- tar_make(callr_function = callr::r_bg, reporter = "silent")
  tar_poll(interval = 0.001, fields = all_of(c("built", "time")))
})

tar_test("tar_poll() timeout", {
  tar_script({
    list(
      tar_target(x, seq_len(100)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  })
  px <- tar_make(callr_function = callr::r_bg, reporter = "silent")
  tar_poll(interval = 0.001, timeout = 5)
})
