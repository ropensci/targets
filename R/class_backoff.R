# Exponential backoff algorithm
# similar to https://en.wikipedia.org/wiki/Exponential_backoff
backoff_init <- function(min = 0.001, max = 0.1, rate = 1.5) {
  backoff_new(
    min = min,
    max = max,
    rate = rate,
    index = 0L
  )
}

backoff_new <- function(
  min = NULL,
  max = NULL,
  rate = NULL,
  index = NULL
) {
  backoff_class$new(
    min = min,
    max = max,
    rate = rate,
    index = index
  )
}

backoff_class <- R6::R6Class(
  classname = "tar_backoff",
  class = TRUE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    min = NULL,
    max = NULL,
    rate = NULL,
    index = NULL,
    initialize = function(
      min = NULL,
      max = NULL,
      rate = NULL,
      index = NULL
    ) {
      self$min <- min
      self$max <- max
      self$rate <- rate
      self$index <- index
    },
    reset = function() {
      self$index <- 0L
    },
    increment = function() {
      self$index <- min(self$index + 1L, as.integer(1e9))
    },
    bound = function() {
      min(self$max, (self$min) * ((self$rate) ^ (self$index)))
    },
    interval = function() {
      stats::runif(n = 1L, min = self$min, max = self$bound())
    },
    wait = function() {
      Sys.sleep(self$interval())
      self$increment()
    },
    validate = function() {
      tar_assert_scalar(self$min)
      tar_assert_scalar(self$max)
      tar_assert_scalar(self$rate)
      tar_assert_scalar(self$index)
      tar_assert_dbl(self$min)
      tar_assert_dbl(self$max)
      tar_assert_dbl(self$rate)
      tar_assert_int(self$index)
      tar_assert_ge(self$min, sqrt(.Machine$double.eps))
      tar_assert_ge(self$max, self$min)
      tar_assert_ge(self$rate, 1)
      tar_assert_ge(self$index, 0L)
    }
  )
)
