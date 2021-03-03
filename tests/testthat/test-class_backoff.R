test_that("backoff$validate()", {
  expect_silent(backoff_init()$validate())
})

test_that("backoff$increment_base() and backoff$reset()", {
  x <- backoff_init(min = 1, rate = 2)
  expect_equal(x$index, 0L)
  expect_equal(x$interval_base(), x$min)
  map(seq_len(5), ~x$increment())
  expect_equal(x$index, 5L)
  expect_equal(x$interval_base(), 2 ^ 5)
})

test_that("backoff$interval()", {
  set.seed(0)
  x <- backoff_init(min = 1, rate = 2)
  expect_gt(x$interval(), x$interval_base())
  expect_lt(x$interval() - 1, x$interval_base())
  map(seq_len(5), ~x$increment())
  expect_gt(x$interval(), x$interval_base())
  expect_lt(x$interval() - 1, x$interval_base())
})
