# These tests need to sleep, which extends test time.
tar_test("seconds has the right object", {
  build <- build_init(quote(Sys.sleep(5L)), baseenv())
  out <- build$metrics$seconds
  expect_equal(out, 5, tolerance = 1e-2)
})

tar_test("branches complete even if patterns do not", {
  tar_script({
    list(
      tar_target(x, c(0, 100)),
      tar_target(y, Sys.sleep(x), pattern = map(x))
    )
  })
  tar_make() # Terminate before the second branch finishes.
  out <- tar_progress()
  expect_equal(out$progress[out$name == "x"], "built")
  expect_equal(out$progress[out$name == "y"], "running")
  expect_equal(out$progress[out$name == "y_bff916f9"], "built")
  expect_equal(out$progress[out$name == "y_e15e8827"], "running")
  tar_make() # The first branch should skip. Again, terminate early.
  out <- tar_progress()
  expect_equal(nrow(out), 2)
  expect_equal(out$progress[out$name == "y"], "running")
  expect_equal(out$progress[out$name == "y_e15e8827"], "running")
})
