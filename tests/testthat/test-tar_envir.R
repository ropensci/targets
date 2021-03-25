tar_test("tar_envir() automatic default", {
  out <- expect_warning(tar_envir(), class = "condition_deprecate")
  exp <- environment()
  expect_equal(out, exp)
})
