local_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  queue = "parallel",
  reporter = "verbose",
  garbage_collection = FALSE
) {
  local_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    garbage_collection = garbage_collection
  )
}

local_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  garbage_collection = NULL
) {
  local_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    garbage_collection = garbage_collection
  )
}

#' @title Abstract class for local algorithm objects.
#' @aliases tar_local
#' @keywords internal
#' @description Not a user-side R6 class.
local_class <- R6::R6Class(
  classname = "tar_local",
  inherit = active_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Only applies to prelocal objects.
    #' @param target Target object.
    assert_deployment = function(target) {
    },
    #' @description Run a target.
    #' @param name Name of a target to run.
    run_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      self$assert_deployment(target)
      target_prepare(target, self$pipeline, self$scheduler)
      target_run(target)
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      self$unload_transient()
    },
    #' @description Process a target.
    #' @param name Name of a target to process.
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      target_debug(target)
      target_update_depend(target, meta)
      trn(
        target_should_run(target, self$meta),
        self$run_target(name),
        target_skip(
          target,
          self$pipeline,
          self$scheduler,
          self$meta
        )
      )
    },
    #' @description Process the next target.
    process_next = function() {
      self$run_gc()
      self$process_target(self$scheduler$queue$dequeue())
    },
    #' @description Run the algorithm.
    run = function() {
      self$start()
      queue <- self$scheduler$queue
      while (queue$should_dequeue()) {
        self$process_next()
      }
      self$end()
      invisible()
    },
    #' @description Validate the algorithm.
    validate = function() {
      super$validate()
      assert_lgl(self$garbage_collection)
    }
  )
)
