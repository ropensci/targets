tar_test("tar_cancel(TRUE)", {
  for (index in seq_len(2)) {
    pipeline <- pipeline_init(
      list(target_init("x", quote(targets::tar_cancel(TRUE))))
    )
    local <- local_init(pipeline)
    local$run()
    out <- counter_get_names(
      local$scheduler$progress$canceled
    )
    expect_equal(out, "x")
    data <- local$meta$database$read_data()
    expect_equal(nrow(data), 0L)
    progress <- local$scheduler$progress$database$read_data()
    expect_equal(progress$name, c("x", "x"))
    expect_equal(progress$progress, c("started", "canceled"))
    expect_false(file.exists(file.path("_targets", "objects", "x")))
  }
})

tar_test("tar_cancel(FALSE)", {
  pipeline <- pipeline_init(
    list(target_init("x", quote(targets::tar_cancel(FALSE))))
  )
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(
    local$scheduler$progress$canceled
  )
  expect_equal(out, character(0))
  out <- counter_get_names(
    local$scheduler$progress$built
  )
  expect_equal(out, "x")
  expect_true(file.exists(file.path("_targets", "objects", "x")))
})
