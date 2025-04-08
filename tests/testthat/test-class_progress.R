tar_test("progress database subkey", {
  out <- progress_init()
  on.exit(out$database$close())
  expect_equal(
    out$database$key,
    file.path(path_store_default(), "meta", "progress")
  )
})

tar_test("progress$queued", {
  expect_silent(counter_validate(progress_init()$queued))
})

tar_test("progress$dispatched", {
  expect_silent(counter_validate(progress_init()$dispatched))
})

tar_test("progress$completed", {
  expect_silent(counter_validate(progress_init()$completed))
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
  on.exit(progress$database$close())
  progress$assign_queued("x")
  expect_equal(counter_get_names(progress$completed), character(0))
  expect_equal(counter_get_names(progress$queued), "x")
})

tar_test("progress$assign_dispatched()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$assign_queued("x")
  progress$assign_dispatched("x")
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$dispatched), "x")
})

tar_test("progress$assign_skipped()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$assign_queued("x")
  progress$assign_skipped("x")
  expect_equal(counter_get_names(progress$queued), character(0))
  expect_equal(counter_get_names(progress$skipped), "x")
})

tar_test("progress$assign_completed()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$assign_dispatched("x")
  progress$assign_completed("x")
  expect_equal(counter_get_names(progress$dispatched), character(0))
  expect_equal(counter_get_names(progress$completed), "x")
})

tar_test("progress$assign_canceled()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$assign_dispatched("x")
  progress$assign_canceled("x")
  expect_equal(counter_get_names(progress$dispatched), character(0))
  expect_equal(counter_get_names(progress$canceled), "x")
})

tar_test("progress$assign_errored()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$assign_dispatched("x")
  progress$assign_errored("x")
  expect_equal(counter_get_names(progress$dispatched), character(0))
  expect_equal(counter_get_names(progress$errored), "x")
})

tar_test("progress$register_dispatched()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$database$reset_storage()
  progress$register_dispatched(target_init("x", 1))
  expect_equal(counter_get_names(progress$dispatched), "x")
  progress$database$flush_rows()
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "dispatched"
  )
  expect_equal(data, exp)
})

tar_test("progress$register_completed()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$database$reset_storage()
  progress$register_completed(target_init("x", 1))
  expect_equal(counter_get_names(progress$completed), "x")
  progress$database$flush_rows()
  data <- progress$database$read_data()
  exp <- data_frame(
    name = "x",
    type = "stem",
    parent = "x",
    branches = 0L,
    progress = "completed"
  )
  expect_equal(data, exp)
})

tar_test("progress$register_canceled()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  progress$database$reset_storage()
  progress$register_canceled(target_init("x", 1))
  expect_equal(counter_get_names(progress$canceled), "x")
  progress$database$flush_rows()
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
  on.exit(progress$database$close())
  progress$database$reset_storage()
  progress$register_errored(target_init("x", 1))
  expect_equal(counter_get_names(progress$errored), "x")
  progress$database$flush_rows()
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
  on.exit(progress$database$close())
  expect_false(progress$any_remaining())
  progress$assign_queued("x")
  expect_true(progress$any_remaining())
})

tar_test("progress$any_targets()", {
  progress <- progress_init()
  on.exit(progress$database$close())
  expect_false(progress$any_targets())
  counter_set_name(progress$completed, "x")
  expect_true(progress$any_targets())
})

tar_test("progress$any_remaining() while dispatched", {
  progress <- progress_init()
  on.exit(progress$database$close())
  expect_false(progress$any_remaining())
  progress$assign_dispatched("x")
  expect_true(progress$any_remaining())
})

tar_test("progress database gets used", {
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  local$end()
  out <- local$scheduler$progress$database$read_data()
  exp <- data_frame(
    name = c("x", "x", "y", "y"),
    type = rep("stem", 4L),
    parent = c("x", "x", "y", "y"),
    branches = rep(0L, 4L),
    progress = c("dispatched", "completed", "dispatched", "completed")
  )
  expect_equal(out, exp)
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- local$scheduler$progress$database$read_data()
  exp <- c("name", "type", "parent", "branches", "progress")
  expect_equal(colnames(out), exp)
  expect_equal(nrow(out), 2L)
  expect_equal(out$progress, rep("skipped", 2L))
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
  expect_equal(branches$progress, rep("completed", 2))
  x <- out[out$name == "x", ]
  y <- out[out$name == "y", ]
  nonbranches <- rbind(x, y)
  expect_equal(nonbranches$name, c("x", "y"))
  expect_equal(nonbranches$type, c("stem", "pattern"))
  expect_equal(nonbranches$parent, c("x", "y"))
  expect_equal(nonbranches$branches, c(0L, 2L))
  expect_equal(nonbranches$progress, rep("completed", 2))
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
