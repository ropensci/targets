tar_test("local$meta", {
  local <- local_init(pipeline_order())
  expect_silent(local$meta$validate())
  expect_false(file.exists("_targets"))
})

tar_test("local_init()$run() stores correct values", {
  pipeline <- pipeline_order()
  local_init(pipeline)$run()
  out <- target_read_value(pipeline_get_target(pipeline, "data1"))$object
  expect_equal(out, seq_len(10))
  out <- target_read_value(pipeline_get_target(pipeline, "data2"))$object
  expect_equal(out, seq_len(20))
  out <- target_read_value(pipeline_get_target(pipeline, "min1"))$object
  expect_equal(out, 1L)
  out <- target_read_value(pipeline_get_target(pipeline, "min2"))$object
  expect_equal(out, 1L)
  out <- target_read_value(pipeline_get_target(pipeline, "max1"))$object
  expect_equal(out, 10L)
  out <- target_read_value(pipeline_get_target(pipeline, "max2"))$object
  expect_equal(out, 20L)
  out <- target_read_value(pipeline_get_target(pipeline, "mins"))$object
  expect_equal(out, c(1L, 1L))
  out <- target_read_value(pipeline_get_target(pipeline, "maxes"))$object
  expect_equal(out, c(10L, 20L))
  out <- target_read_value(pipeline_get_target(pipeline, "all"))$object
  expect_equal(out, c(1L, 1L, 10L, 20L))
})

tar_test("all targets unloaded at end", {
  x <- target_init("x", quote(1), memory = "persistent")
  y <- target_init("y", quote(1), memory = "transient")
  pipeline <- pipeline_init(list(x, y))
  local_init(pipeline)$run()
  expect_null(x$value)
  expect_null(y$value)
})

tar_test("transient targets unloaded periodically", {
  x <- target_init("x", quote(1), memory = "persistent")
  y <- target_init("y", quote(1), memory = "transient")
  pipeline <- pipeline_init(list(x, y))
  local_init(pipeline)$run()
  expect_null(x$value)
  expect_null(y$value)
})

tar_test("can run on a subset of targets", {
  pipeline <- pipeline_order()
  local <- local_init(pipeline, names = c("min1", "max2"))
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- c("min1", "max2", "data1", "data2")
  expect_equal(sort(out), sort(exp))
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "data1"))$object,
    seq_len(10)
  )
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "data2"))$object,
    seq_len(20)
  )
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "min1"))$object,
    1L
  )
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "max2"))$object,
    20L
  )
})

tar_test("local$validate()", {
  local <- local_init(pipeline_order())
  local$ensure_process()
  expect_silent(local$validate())
})
