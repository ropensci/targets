#' @title Active algorithm constructor.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#'   Used to build external HPC scheduling algorithms.
#' @param pipeline Pipeline object.
#' @param meta Meta object.
#' @param names Character, names of targets.
#' @param queue Character, type of queue.
#' @param reporter Character, type of reporter.
#' @param garbage_collection Logical, whether to
#'   periodically run garbage collection.
active_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  garbage_collection = NULL
) {
  active_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    garbage_collection = garbage_collection
  )
}

#' @title Abstract class for active algorithm objects.
#' @export
#' @keywords internal
#' @description Not a user-side R6 class.
#'   Used to build external HPC scheduling algorithms.
active_class <- R6::R6Class(
  classname = "tar_active",
  inherit = algorithm_class,
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
    #' @field garbage_collection Logical, whether to
    #'   periodically run garbage collection.
    garbage_collection = NULL,
    #' @description Initialize an active algorithm object.
    #' @param pipeline Pipeline object.
    #' @param meta Meta object.
    #' @param names Character, names of targets.
    #' @param queue Character, type of queue.
    #' @param reporter Character, type of reporter.
    #' @param garbage_collection Logical, whether to
    #'   periodically run garbage collection.
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      garbage_collection = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter
      )
      self$garbage_collection <- garbage_collection
    },
    #' @description Ensure metadata is pre-processed
    #'   and up to date in memory.
    ensure_meta = function() {
      self$meta$database$preprocess(write = TRUE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$record_imports(envir, self$pipeline)
    },
    #' @description Run garbage collection if enabled.
    run_gc = function() {
      if (self$garbage_collection) {
        gc()
      }
    },
    #' @description Unload transient-memory targets.
    unload_transient = function() {
      pipeline_unload_transient(self$pipeline)
    },
    #' @description Unserialize a target.
    #' @param target Target object.
    unserialize_target = function(target) {
      builder_unserialize_value(target)
    },
    #' @description Start the algorithm.
    start = function() {
      pipeline_prune_names(self$pipeline, self$names)
      self$update_scheduler()
      self$ensure_meta()
      self$scheduler$progress$database$reset_storage()
      self$scheduler$reporter$report_start()
    },
    #' @description End the algorithm.
    end = function() {
      pipeline_unload_loaded(self$pipeline)
      scheduler <- self$scheduler
      scheduler$reporter$report_end(scheduler$progress)
      store_del_scratch()
      self$run_gc()
    },
    #' @description Validate the algorithm.
    validate = function() {
      super$validate()
      assert_lgl(self$garbage_collection)
    }
  )
)
