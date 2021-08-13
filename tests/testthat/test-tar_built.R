tar_test("tar_built() empty", {
  expect_equal(tar_built(), character(0))
  expect_equal(tar_built(contains("x")), character(0))
})

tar_test("tar_built() nonempty", {
  skip_on_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  expect_equal(sort(tar_built()), sort(c("x", "y")))
  expect_equal(tar_built(contains("x")), "x")
})
