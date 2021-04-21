tar_test("path_scratch_fixed()", {
  expect_equal(path_scratch_fixed("x"), file.path("_targets", "scratch", "x"))
})
