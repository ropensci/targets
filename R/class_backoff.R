# Exponential backoff algorithm
# similar to https://en.wikipedia.org/wiki/Exponential_backoff
backoff_init <- function(start = 0.01, max = 20, rate = 2) {
  backoff_new(
    start = start,
    max = max,
    rate = rate,
    interval = start
  )
}

backoff_new <- function(
  start = NULL,
  max = NULL,
  rate = NULL,
  interval = NULL
) {
  backoff_class$new(
    start = start,
    max = max,
    rate = rate,
    interval = interval
  )
}

backoff_class <- R6::R6Class(
  classname = "tar_backoff",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    start = NULL,
    max = NULL,
    rate = NULL,
    interval = NULL,
    initialize = function(
      start = NULL,
      max = NULL,
      rate = NULL,
      interval = NULL
    ) {
      self$start <- start
      self$max <- max
      self$rate <- rate
      self$interval <- interval
    },
    validate = function() {
      assert_scalar(self$start)
      assert_scalar(self$max)
      assert_scalar(self$rate)
      assert_scalar(self$interval)
      assert_dbl(self$start)
      assert_dbl(self$max)
      assert_dbl(self$rate)
      assert_dbl(self$interval)
      assert_ge(self$start, 0)
      assert_ge(self$max, self$start)
      assert_ge(self$rate, 1)
      assert_ge(self$interval, self$start)
      assert_ge(self$max, self$interval)
    }
  )
)
