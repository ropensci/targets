tar_test("tar_pid()", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  expect_equal(tar_pid(), Sys.getpid())
})
