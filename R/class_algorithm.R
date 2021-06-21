algorithm_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL
) {
  algorithm_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter
  )
}

algorithm_class <- R6::R6Class(
  classname = "tar_algorithm",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    meta = NULL,
    scheduler = NULL,
    names = NULL,
    shortcut = NULL,
    queue = NULL,
    reporter = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      shortcut = NULL,
      queue = NULL,
      reporter = NULL
    ) {
      self$pipeline <- pipeline
      self$meta <- meta
      self$names <- names
      self$shortcut <- shortcut
      self$queue <- queue
      self$reporter <- reporter
    },
    update_scheduler = function() {
      self$scheduler <- scheduler_init(
        pipeline = self$pipeline,
        meta = self$meta,
        queue = self$queue,
        reporter = self$reporter,
        names = self$names,
        shortcut = self$shortcut
      )
    },
    bootstrap_shortcut_deps = function() {
      if (!is.null(self$names) && self$shortcut) {
        pipeline_bootstrap_deps(self$pipeline, self$meta, self$names)
      }
    },
    validate = function() {
      pipeline_validate(self$pipeline)
      (self$scheduler %|||% scheduler_init())$validate()
      self$meta$validate()
      tar_assert_chr(self$names %|||% character(0))
      tar_assert_chr(self$queue %|||% character(0))
      tar_assert_chr(self$reporter %|||% character(0))
    }
  )
)
