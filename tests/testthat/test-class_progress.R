tar_test("progress$queued", {
  expect_silent(counter_validate(progress_init()$queued))
})

tar_test("progress$running", {
  expect_silent(counter_validate(progress_init()$running))
})

tar_test("progress$built", {
  expect_silent(counter_validate(progress_init()$built))
})

tar_test("progress$skipped", {
  expect_silent(counter_validate(progress_init()$skipped))
})

tar_test("progress$cancelled", {
  expect_silent(counter_validate(progress_init()$cancelled))
})

tar_test("progress$errored", {
  expect_silent(counter_validate(progress_init()$errored))
})

tar_test("progress$assign_queued()", {
  progress <- progress_init()
  progress$assign_queued("x")
  expect_equal(counter_get_names(progress$built), character(0))
  expect_equal(counter_get_names(progress$queued), "x")
})

tar_test("progress$assign_running()", {
  progress <- progress_init()
  progress$assign_queued("x")
  progress$assign_running("x")
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$running), "x")
})

tar_test("progress$assign_skipped()", {
  progress <- progress_init()
  progress$assign_queued("x")
  progress$assign_skipped("x")
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$skipped), "x")
})

tar_test("progress$assign_built()", {
  progress <- progress_init()
  progress$assign_running("x")
  progress$assign_built("x")
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$built), "x")
})

tar_test("progress$assign_cancelled()", {
  progress <- progress_init()
  progress$assign_running("x")
  progress$assign_cancelled("x")
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$cancelled), "x")
})

tar_test("progress$assign_errored()", {
  progress <- progress_init()
  progress$assign_running("x")
  progress$assign_errored("x")
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$errored), "x")
})

tar_test("progress$register_running()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_running("x")
  expect_equal(counter_get_names(progress$running), "x")
  data <- progress$database$read_data()
  expect_equal(data, data_frame(name = "x", progress = "running"))
})

tar_test("progress$register_built()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_built("x")
  expect_equal(counter_get_names(progress$built), "x")
  data <- progress$database$read_data()
  expect_equal(data, data_frame(name = "x", progress = "built"))
})

tar_test("progress$register_cancelled()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_cancelled("x")
  expect_equal(counter_get_names(progress$cancelled), "x")
  data <- progress$database$read_data()
  expect_equal(data, data_frame(name = "x", progress = "cancelled"))
})

tar_test("progress$register_errored()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_errored("x")
  expect_equal(counter_get_names(progress$errored), "x")
  data <- progress$database$read_data()
  expect_equal(data, data_frame(name = "x", progress = "errored"))
})

tar_test("progress$any_remaining() while queued", {
  progress <- progress_init()
  expect_false(progress$any_remaining())
  progress$assign_queued("x")
  expect_true(progress$any_remaining())
})

tar_test("progress$any_remaining() while running", {
  progress <- progress_init()
  expect_false(progress$any_remaining())
  progress$assign_running("x")
  expect_true(progress$any_remaining())
})

tar_test("progress database gets used", {
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- local$scheduler$progress$database$read_data()
  exp <- data_frame(
    name = c("x", "x", "y", "y"),
    progress = c("running", "built", "running", "built")
  )
  expect_equal(out, exp)
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- local$scheduler$progress$database$read_data()
  expect_equal(colnames(out), c("name", "progress"))
  expect_equal(nrow(out), 0L)
})

tar_test("progress$validate()", {
  expect_silent(progress_init()$validate())
})
