# Exponential backoff algorithm
# similar to https://en.wikipedia.org/wiki/Exponential_backoff
backoff_init <- function(interval = 0.01, max = 20, rate = 2) {
  backoff_new(interval = interval, max = max, rate = rate)
}

backoff_new <- function(interval = NULL, max = NULL, rate = NULL) {
  force(interval)
  force(max)
  force(rate)
  environment()
}

backoff_validate <- function(backoff) {
  assert_correct_fields(backoff, backoff_new)
  assert_scalar(backoff$interval)
  assert_scalar(backoff$max)
  assert_scalar(backoff$rate)
  assert_dbl(backoff$interval)
  assert_dbl(backoff$max)
  assert_dbl(backoff$rate)
  assert_ge(backoff$interval, 0)
  assert_ge(backoff$max, backoff$interval)
  assert_ge(backoff$rate, 1)
}
