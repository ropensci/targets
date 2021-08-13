tar_test("tar_canceled() empty", {
  expect_equal(tar_canceled(), character(0))
  expect_equal(tar_canceled(contains("x")), character(0))
})

tar_test("tar_canceled() nonempty", {
  skip_on_cran()
  tar_script(list(tar_target(x, tar_cancel()), tar_target(y, tar_cancel())))
  tar_make(callr_function = NULL)
  expect_equal(sort(tar_canceled()), sort(c("x", "y")))
  expect_equal(tar_canceled(contains("x")), "x")
})
