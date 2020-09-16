# Needs to sleep, which extends test time.
tar_test("seconds has the right object", {
  build <- build_init(quote(Sys.sleep(5L)), baseenv())
  out <- build$metrics$seconds
  expect_equal(out, 5, tolerance = 1e-2)
})
