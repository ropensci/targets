tar_test("tar_pipeline() works with loose targets", {
  a <- tar_target(a, "a")
  b <- tar_target(b, c(a, "b"))
  expect_warning(
    pipeline <- tar_pipeline(a, b),
    class = "tar_condition_deprecate"
  )
  expect_silent(pipeline_validate(pipeline))
  local_init(pipeline = pipeline)$run()
  expect_equal(target_read_value(b)$object, c("a", "b"))
})

tar_test("tar_pipeline() works with target lists", {
  expect_warning(
    pipeline <- tar_pipeline(
      list(
        tar_target(a, "a"),
        tar_target(b, c(a, "b"))
      )
    ),
    class = "tar_condition_deprecate"
  )
  expect_silent(pipeline_validate(pipeline))
  local_init(pipeline = pipeline)$run()
  b <- pipeline_get_target(pipeline, "b")
  expect_equal(target_read_value(b)$object, c("a", "b"))
})

tar_test("tar_pipeline() works with weird lists", {
  expect_warning(
    pipeline <- tar_pipeline(
      list(
        tar_target(ct, c(b, "c")),
        tar_target(d, c(ct, "d"))
      ),
      tar_target(e, c(d, "e")),
      list(
        tar_target(a, "a"),
        tar_target(b, c(a, "b"))
      )
    ),
    class = "tar_condition_deprecate"
  )
  expect_silent(pipeline_validate(pipeline))
  local_init(pipeline = pipeline)$run()
  e <- pipeline_get_target(pipeline, "e")
  expect_equal(target_read_value(e)$object, c("a", "b", "c", "d", "e"))
})
