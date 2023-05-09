tar_test("backoff$validate()", {
  expect_silent(tar_backoff()$validate())
})

tar_test("index and bound", {
  x <- tar_backoff(min = 1, rate = 2, max = 50)
  expect_equal(x$index, 0L)
  expect_equal(x$bound(), x$min)
  for (index in seq_len(5L)) {
    x$increment()
    expect_equal(x$index, index)
    expect_equal(x$bound(), 2 ^ index)
  }
  x$reset()
  expect_equal(x$index, 0L)
  expect_equal(x$bound(), x$min)
})

tar_test("maximum interval", {
  set.seed(0)
  x <- tar_backoff(min = 1, rate = 2, max = 10)
  map(seq_len(100), ~x$increment())
  expect_equal(x$index, 100L)
  expect_equal(x$bound(), 10)
})

tar_test("backoff$wait() increments index and bound", {
  set.seed(0)
  x <- tar_backoff(min = 0.001, rate = 2, max = 10)
  expect_equal(x$index, 0L)
  expect_equal(x$bound(), 0.001)
  x$wait()
  expect_equal(x$index, 1L)
  expect_equal(x$bound(), 0.002)
})
