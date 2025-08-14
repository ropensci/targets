tar_test("backoff actually slows down and then resets", {
  test_backoff <- function(backoff) {
    expect_equal(backoff$index, 0L)
    begin <- unname(proc.time()["elapsed"])
    for (index in seq_len(30L)) {
      bound <- round(backoff$bound(), 4L)
      backoff$wait()
      end <- unname(proc.time()["elapsed"])
      elapsed <- end - begin
      begin <- end
      msg <- paste0(
        "bound: ",
        bound,
        " | elapsed: ",
        round(elapsed, 4),
        "\n"
      )
      cat(msg)
    }
  }
  backoff <- backoff_init(min = 0.001, max = 2, rate = 1.5)
  # Should slow down exponentially.
  test_backoff(backoff)
  # Reset the backoff.
  backoff$reset()
  cat("\n")
  # Should have reset the slowdown.
  test_backoff(backoff)
})

tar_test("backoff interval is uniformly distributed", {
  set.seed(1)
  backoff <- backoff_init(min = 1, max = 10, rate = 2)
  map(seq_len(1e3), ~ backoff$increment())
  out <- map_dbl(seq_len(1e5), ~ backoff$interval())
  # uniform between 1 and 10
  expect_equal(min(out), 1, tolerance = 1e-3)
  expect_equal(max(out), 10, tolerance = 1e-3)
  hist(out)
})
