tar_test("tar_seed() outside a pipeline", {
  expect_equal(tar_seed(), 1L)
  expect_equal(tar_seed(default = 123L), 123L)
})

tar_test("tar_seed() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_seed(default = 1L)))
  local_init(pipeline_init(list(x)))$run()
  expect_true(is.integer(target_read_value(x)$object))
  seed <- target_read_value(x)$object
  expect_false(seed == 1L)
  tar_destroy()
  x <- target_init("x", quote(targets::tar_seed(default = 1L)))
  local_init(pipeline_init(list(x)))$run()
  expect_identical(target_read_value(x)$object, seed)
})
