tar_test("tar_poll() with default columns", {
  tar_script({
    list(
      tar_target(x, seq_len(100)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  })
  tar_poll()
  # Open a separate R session and run the pipeline there.
  # Watch the output of tar_poll().
  # Then stop and destroy the pipeline and watch tar_poll() respond.
  # Then resume the pipeline and watch tar_poll() resume.
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

tar_test("tar_poll() with non-default store and script", {
  tar_script({
    list(
      tar_target(x, seq_len(100)),
      tar_target(y, Sys.sleep(0.1), pattern = map(x))
    )
  }, script = "example/script.R")
  px <- tar_make(
    callr_function = callr::r_bg,
    reporter = "silent",
    script = "example/script.R",
    store = "example/store"
  )
  # Stop after you see it is working:
  tar_poll(interval = 0.001, store = "example/store")
  px$kill()
  expect_true(file.exists("example/store"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
  unlink("example", recursive = TRUE)
})
