tar_test("tar_name() outside a pipeline", {
  expect_equal(tar_name(), "target")
  expect_equal(tar_name(default = "custom"), "custom")
})

tar_test("tar_name() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_name()))
  local_init(pipeline_init(list(x)))$run()
  expect_equal(target_read_value(x)$object, "x")
})
