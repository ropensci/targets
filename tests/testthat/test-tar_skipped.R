tar_test("tar_skipped() empty", {
  expect_equal(tar_skipped(), character(0))
  expect_equal(tar_skipped(contains("x")), character(0))
})

tar_test("tar_skipped() nonempty", {
  skip_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  tar_make(callr_function = NULL)
  expect_equal(sort(tar_skipped()), sort(c("x", "y")))
  expect_equal(tar_skipped(contains("x")), "x")
})
