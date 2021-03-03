tar_test("backoff actually slows down and then resets", {
  test_backoff <- function(backoff) {
    expect_equal(backoff$index, 0L)
    begin <- unname(proc.time()["elapsed"])
    for (index in seq_len(30L)) {
      bound <- round(backoff$bound(), 4L)
      interval <- round(backoff$interval(), 4L)
      backoff$sleep()
      end <- unname(proc.time()["elapsed"])
      elapsed <- end - begin
      begin <- end
      msg <- paste0(
        "bound: ",
        bound,
        " | interval: ",
        interval,
        " | elapsed: ",
        round(elapsed, 4),
        "\n"
      )
      cat(msg)
    }
  }
  backoff <- backoff_init(min = 0.01, max = 2, rate = 1.5)
  # Should slow down exponentially.
  test_backoff(backoff)
  # Reset the backoff.
  backoff$reset()
  cat()
  # Should have reset the slowdown.
  test_backoff(backoff)
})
