tar_test("tar_objects()", {
  expect_equal(tar_objects(), character(0))
  tar_script({
    list(
      tar_target(x, "value"),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_objects(), sort(c("x", "y")))
})
