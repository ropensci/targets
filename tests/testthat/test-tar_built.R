tar_test("tar_built() empty", {
  expect_warning(tar_built(), class = "tar_condition_deprecate")
  expect_equal(suppressWarnings(tar_built()), character(0))
  expect_equal(suppressWarnings(tar_built(contains("x"))), character(0))
})

tar_test("tar_built() nonempty", {
  skip_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  expect_equal(sort(suppressWarnings(tar_built())), sort(c("x", "y")))
  expect_equal(suppressWarnings(tar_built(contains("x"))), "x")
})
