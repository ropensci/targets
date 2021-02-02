tar_test("tar_objects()", {
  expect_equal(tar_objects(), character(0))
  tar_script({
    list(
      tar_target(x1, "value"),
      tar_target(x2, "value"),
      tar_target(y, x1)
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_objects(), sort(c("x1", "x2", "y")))
  expect_equal(
    tar_objects(starts_with("x")),
    sort(c("x1", "x2"))
  )
})
