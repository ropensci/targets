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

#' @title Abstract class for algorithm objects.
#' @aliases tar_algorithm
#' @keywords internal
#' @description Not a user-side R6 class.
algorithm_class <- R6::R6Class(
  classname = "tar_algorithm",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field pipeline Pipeline object.
    pipeline = NULL,
    #' @field meta Meta object.
    meta = NULL,
    #' @field scheduler Scheduler object.
    scheduler = NULL,
    #' @field names Character, names of targets.
    names = NULL,
    #' @field queue Character, name of the queue type.
    queue = NULL,
    #' @field reporter Character, name of the reporter.
    reporter = NULL,
    #' @description Initialize an active algorithm object.
    #' @param pipeline Pipeline object.
    #' @param meta Meta object.
    #' @param names Character, names of targets.
    #' @param queue Character, type of queue.
    #' @param reporter Character, type of reporter.
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
    #' @description Create the scheduler from the
    #'   pipeline and settings.
    update_scheduler = function() {
      self$scheduler <- pipeline_produce_scheduler(
        self$pipeline,
        self$queue,
        self$reporter
      )
    },
    #' @description Validate the algorithm.
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
