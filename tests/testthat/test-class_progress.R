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

tar_test("progress$canceled", {
  expect_silent(counter_validate(progress_init()$canceled))
})

tar_test("progress$errored", {
  expect_silent(counter_validate(progress_init()$errored))
})

tar_test("progress$assign_queued()", {
  progress <- progress_init()
  progress$assign_queued(target_init("x", 1))
  expect_equal(counter_get_names(progress$built), character(0))
  expect_equal(counter_get_names(progress$queued), "x")
})

tar_test("progress$assign_running()", {
  progress <- progress_init()
  progress$assign_queued(target_init("x", 1))
  progress$assign_running(target_init("x", 1))
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$running), "x")
})

tar_test("progress$assign_skipped()", {
  progress <- progress_init()
  progress$assign_queued(target_init("x", 1))
  progress$assign_skipped(target_init("x", 1))
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$skipped), "x")
})

tar_test("progress$assign_built()", {
  progress <- progress_init()
  progress$assign_running(target_init("x", 1))
  progress$assign_built(target_init("x", 1))
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$built), "x")
})

tar_test("progress$assign_canceled()", {
  progress <- progress_init()
  progress$assign_running(target_init("x", 1))
  progress$assign_canceled(target_init("x", 1))
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$canceled), "x")
})

tar_test("progress$assign_errored()", {
  progress <- progress_init()
  progress$assign_running(target_init("x", 1))
  progress$assign_errored(target_init("x", 1))
  expect_equal(counter_get_names(progress$running), character(0))
  expect_equal(counter_get_names(progress$errored), "x")
})

tar_test("progress$register_running()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_running(target_init("x", 1))
  expect_equal(counter_get_names(progress$running), "x")
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "running"
  )
  expect_equal(data, exp)
})

tar_test("progress$register_built()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_built(target_init("x", 1))
  expect_equal(counter_get_names(progress$built), "x")
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "built"
  )
  expect_equal(data, exp)
})

tar_test("progress$register_canceled()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_canceled(target_init("x", 1))
  expect_equal(counter_get_names(progress$canceled), "x")
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "canceled"
  )
  expect_equal(data, exp)
})

tar_test("progress$register_errored()", {
  progress <- progress_init()
  progress$database$reset_storage()
  progress$register_errored(target_init("x", 1))
  expect_equal(counter_get_names(progress$errored), "x")
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "errored"
  )
  expect_equal(data, exp)
})

tar_test("progress$any_remaining() while queued", {
  progress <- progress_init()
  expect_false(progress$any_remaining())
  progress$assign_queued(target_init("x", 1))
  expect_true(progress$any_remaining())
})

tar_test("progress$any_remaining() while running", {
  progress <- progress_init()
  expect_false(progress$any_remaining())
  progress$assign_running(target_init("x", 1))
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
    type = rep("stem", 4L),
    parent = c("x", "x", "y", "y"),
    branches = rep(0L, 4L),
    progress = c("running", "built", "running", "built")
  )
  expect_equal(out, exp)
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- local$scheduler$progress$database$read_data()
  exp <- c("name", "type", "parent", "branches", "progress")
  expect_equal(colnames(out), exp)
  expect_equal(nrow(out), 0L)
})

tar_test("progress records dynamic branching data", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, x, pattern = map(x))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_progress(fields = NULL)
  branches <- out[out$type == "branch", ]
  expect_equal(dim(branches), c(2L, 5L))
  expect_true(all(grepl("^y_", branches$name)))
  expect_equal(branches$type, rep("branch", 2))
  expect_equal(branches$parent, rep("y", 2))
  expect_equal(branches$branches, rep(0L, 2))
  expect_equal(branches$progress, rep("built", 2))
  x <- out[out$name == "x", ]
  y <- out[out$name == "y", ]
  nonbranches <- rbind(x, y)
  expect_equal(nonbranches$name, c("x", "y"))
  expect_equal(nonbranches$type, c("stem", "pattern"))
  expect_equal(nonbranches$parent, c("x", "y"))
  expect_equal(nonbranches$branches, c(0L, 2L))
  expect_equal(nonbranches$progress, rep("built", 2))
})

tar_test("progress records dynamic branching error status", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, stopifnot(x < 1.5), pattern = map(x))
    )
  )
  expect_error(tar_make(callr_function = NULL))
  out <- tar_progress(fields = NULL)
  x <- out[out$progress == "errored" & out$type == "pattern", ]
  expect_equal(x$name, "y")
  expect_equal(x$type, "pattern")
  expect_equal(x$parent, "y")
  expect_equal(x$branches, 2L)
  expect_equal(x$progress, "errored")
  x <- out[out$progress == "errored" & out$type == "branch", ]
  expect_true(all(grepl("^y_", x$name)))
  expect_equal(x$type, "branch")
  expect_equal(x$parent, "y")
  expect_equal(x$branches, 0L)
  expect_equal(x$progress, "errored")
})

tar_test("progress$validate()", {
  expect_silent(progress_init()$validate())
})
