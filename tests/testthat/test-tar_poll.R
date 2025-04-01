tar_test("tar_poll()", {
  skip_cran()
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL, reporter = "silent")
  tar_poll(interval = 0.001, timeout = 0.25)
  expect_true(TRUE)
})
