tar_test("tar_stage() outside a pipeline with no arguments", {
  expect_true(is.character(tar_stage()))
  expect_true(is.na(tar_stage()))
  expect_equal(length(tar_stage()), 1L)
  expect_equal(tar_stage(default = "x"), "x")
})

tar_test("tar_stage() inside a pipeline", {
  x <- target_init("x", quote(targets::tar_stage()))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  path <- file.path("_targets", "scratch")
  expect_equal(dirname(target_read_value(x, pipeline)$object), path)
})
