tar_test("tar_name() outside a pipeline", {
  expect_error(tar_name(), class = "condition_validate")
})

tar_test("tar_name() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_name()))
  algorithm_init("local", pipeline_init(list(x)))$run()
  expect_equal(target_read_value(x)$object, "x")
})
