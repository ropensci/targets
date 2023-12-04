tar_test("tar_completed() empty", {
  expect_equal(tar_completed(), character(0))
  expect_equal(tar_completed(contains("x")), character(0))
})

tar_test("tar_completed() nonempty", {
  skip_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  expect_equal(sort(tar_completed()), sort(c("x", "y")))
  expect_equal(tar_completed(contains("x")), "x")
})
