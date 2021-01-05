tar_test("tar_path() outside a pipeline with no arguments", {
  expect_true(is.character(tar_path()))
  expect_true(is.na(tar_path()))
  expect_equal(length(tar_path()), 1L)
  expect_equal(tar_path(default = "x"), "x")
})

tar_test("tar_path() with a name arg", {
  expect_equal(tar_path(x), file.path("_targets", "objects", "x"))
})

tar_test("tar_path() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_path()))
  local_init(pipeline_init(list(x)))$run()
  path <- file.path("_targets", "objects", "x")
  expect_equal(target_read_value(x)$object, path)
})
