algorithm_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL
) {
  algorithm_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter
  )
}

algorithm_class <- R6::R6Class(
  classname = "tar_algorithm",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    meta = NULL,
    scheduler = NULL,
    names = NULL,
    queue = NULL,
    reporter = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL
    ) {
      self$pipeline <- pipeline
      self$meta <- meta
      self$names <- names
      self$queue <- queue
      self$reporter <- reporter
    },
    update_scheduler = function() {
      self$scheduler <- pipeline_produce_scheduler(
        self$pipeline,
        self$queue,
        self$reporter
      )
    },
    validate = function() {
      pipeline_validate(self$pipeline)
      (self$scheduler %||% scheduler_init())$validate()
      self$meta$validate()
      assert_chr(self$names %||% character(0))
      assert_chr(self$queue %||% character(0))
      assert_chr(self$reporter %||% character(0))
    }
  )
)
