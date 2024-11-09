tar_test("reference with only parent", {
  out <- reference_new(parent = "my_parent")
  expect_equal(out, c(parent = "my_parent"))
})

tar_test("reference with parent and path", {
  out <- reference_new(parent = "my_parent", path = "my_path")
  expect_equal(out, c(parent = "my_parent", path = "my_path"))
})

tar_test("reference with parent and stage", {
  out <- reference_new(parent = "my_parent", stage = "my_stage")
  expect_equal(out, c(parent = "my_parent", stage = "my_stage"))
})

tar_test("reference with parent and path", {
  out <- reference_new(
    parent = "my_parent",
    path = "my_path",
    stage = "my_stage"
  )
  expect_equal(
    out,
    c(parent = "my_parent", path = "my_path", stage = "my_stage")
  )
})
