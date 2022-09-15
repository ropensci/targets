tar_test("tar_errored() empty", {
  expect_equal(tar_errored(), character(0))
  expect_equal(tar_errored(contains("x")), character(0))
})

tar_test("tar_errored() nonempty", {
  skip_cran()
  tar_script({
    tar_option_set(error = "continue")
    list(tar_target(x, stop()), tar_target(y, stop()))
  })
  suppressWarnings(tar_make(callr_function = NULL))
  expect_equal(sort(tar_errored()), sort(c("x", "y")))
  expect_equal(tar_errored(contains("x")), "x")
})
