tar_test("progress of successful pattern", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  progress <- local$scheduler$progress$database$read_data()
  progress <- as_data_frame(progress)
  progress <- progress[progress$name == "map", ]
  expect_equal(progress$progress, c("running", "built"))
})

tar_test("progress of a pattern with a cancelled branch", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(targets::tar_cancel(data > 2L)),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  progress <- local$scheduler$progress$database$read_data()
  progress <- as_data_frame(progress)
  progress <- progress[progress$name == "map", ]
  expect_equal(progress$progress, c("running", "cancelled"))
})

tar_test("progress of a pattern with a errored branch", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(stopifnot(data < 3L)),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
  progress <- local$scheduler$progress$database$read_data()
  progress <- as_data_frame(progress)
  progress <- progress[progress$name == "map", ]
  expect_equal(progress$progress, c("running", "errored"))
})

tar_test("same, but continue on error", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(stopifnot(data > 1L)),
        pattern = quote(map(data)),
        error = "continue"
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  progress <- local$scheduler$progress$database$read_data()
  progress <- as_data_frame(progress)
  progress <- progress[progress$name == "map", ]
  expect_equal(progress$progress, c("running", "errored"))
})

tar_test("patternview_validate()", {
  expect_silent(patternview_validate(patternview_init()))
})
