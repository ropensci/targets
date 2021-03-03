tar_test("backoff actually slows down and then resets", {
  test_backoff <- function(backoff) {
    expect_equal(backoff$index, 0L)
    begin <- unname(proc.time()["elapsed"])
    for (index in seq_len(30)) {
      base <- backoff$interval_base()
      backoff$sleep()
      end <- unname(proc.time()["elapsed"])
      elapsed <- end - begin
      begin <- end
      msg <- paste0(
        "base interval: ",
        round(base, 4),
        " | elapsed: ",
        round(elapsed, 4),
        "\n"
      )
      cat(msg)
    }
    expect_equal(backoff$index, 30L)
  }
  backoff <- backoff_init(min = 0.01, max = 2, rate = 1.25)
  # Should slow down exponentially.
  test_backoff(backoff)
  # Reset the backoff.
  backoff$reset()
  # Should have reset the slowdown.
  test_backoff(backoff)
})
