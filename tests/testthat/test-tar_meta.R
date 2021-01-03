tar_test("tar_meta() works", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_meta()
  expect_equal(nrow(out), 1L)
  expect_equal(out$name, "x")
  expect_equal(colnames(out), header_meta())
})

tar_test("tar_meta() with target selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_meta()
  expect_equal(nrow(out), 4L)
  out <- tar_meta(c("y", "x"))
  expect_equal(out$name, c("y", "x"))
  out <- tar_meta(c("x", "y"))
  expect_equal(out$name, c("x", "y"))
})

tar_test("tar_meta() with field selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_meta(fields = "command")
  expect_equal(nrow(out), 4L)
  expect_equal(colnames(out), c("name", "command"))
})
