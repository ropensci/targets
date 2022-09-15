tar_test("callr_args_default", {
  skip_cran()
  expect_message(out <- callr_args_default(callr::r))
  expect_equal(out, tar_callr_args_default(callr::r))
})
