tar_test("tar_progress() with defaults", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_progress()
  expect_equal(dim(out), c(1L, 2L))
  expect_equal(out$name, "x")
  expect_equal(out$progress, "built")
})

tar_test("tar_progress() with fields = NULL", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_progress(fields = NULL)
  expect_equal(dim(out), c(1L, 5L))
  expect_equal(out$name, "x")
  expect_equal(out$type, "stem")
  expect_equal(out$parent, "x")
  expect_equal(out$branches, 0L)
  expect_equal(out$progress, "built")
})

tar_test("tar_progress() tidyselect", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_progress(fields = type)
  expect_equal(dim(out), c(1L, 2L))
  expect_equal(out$name, "x")
  expect_equal(out$type, "stem")
})

tar_test("tar_progress() with target selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_progress()
  expect_equal(nrow(out), 4L)
  out <- tar_progress(c("y", "x"))
  expect_equal(out$name, c("y", "x"))
  out <- tar_progress(c("x", "y"))
  expect_equal(out$name, c("x", "y"))
})
