tar_test("backoff$validate()", {
  expect_silent(backoff_init()$validate())
})

tar_test("index and intervals", {
  x <- backoff_init(min = 1, rate = 2, max = 50)
  expect_equal(x$index, 0L)
  expect_equal(x$interval_base(), x$min)
  expect_gt(x$interval(), 1)
  expect_lt(x$interval(), 1.11)
  for (index in seq_len(5L)) {
    x$increment()
    expect_equal(x$index, index)
    expect_equal(x$interval_base(), 2 ^ index)
    expect_gt(x$interval(), x$interval_base())
    expect_lt(x$interval() - 1, x$interval_base())
  }
  x$reset()
  expect_equal(x$index, 0L)
  expect_equal(x$interval_base(), x$min)
  expect_gt(x$interval(), 1)
  expect_lt(x$interval(), 1.11)
})

tar_test("maximum base interval", {
  set.seed(0)
  x <- backoff_init(min = 1, rate = 2, max = 10)
  map(seq_len(100), ~x$increment())
  expect_equal(x$interval_base(), 10)
  expect_gt(x$interval(), x$interval_base())
  expect_lt(x$interval() - 1, x$interval_base())
})

tar_test("sleep increments index", {
  set.seed(0)
  x <- backoff_init(min = 0.001, rate = 2, max = 10)
  expect_equal(x$index, 0L)
  expect_equal(x$interval_base(), 0.001)
  expect_gt(x$interval(), x$interval_base())
  expect_lt(x$interval() - 1, x$interval_base())
  x$sleep()
  expect_equal(x$index, 1L)
  expect_equal(x$interval_base(), 0.002)
  expect_gt(x$interval(), x$interval_base())
  expect_lt(x$interval() - 1, x$interval_base())
})
