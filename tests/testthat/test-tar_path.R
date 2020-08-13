tar_test("tar_path() outside a pipeline with no arguments", {
  expect_error(tar_path(), class = "condition_validate")
})

tar_test("tar_path() with a name arg", {
  exp <- file.path("_targets", "objects", "x")
  expect_equal(tar_path(x), exp)
})

tar_test("tar_path() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_path()))
  local_init(pipeline_init(list(x)))$run()
  path <- file.path("_targets", "objects", "x")
  expect_equal(target_read_value(x)$object, path)
})
