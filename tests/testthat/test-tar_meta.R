tar_test("tar_meta() works", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  algorithm_init("local", pipeline = pipeline)$run()
  out <- tar_meta()
  expect_equal(nrow(out), 1L)
  expect_equal(out$name, "x")
  expect_equal(colnames(out), header_meta())
})

tar_test("tar_meta() with target selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_options(envir = envir)
    tar_pipeline(
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
