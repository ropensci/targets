#' @title Prelocal algorithm constructor.
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
#' @param scheduler Scheduler object.
prelocal_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  garbage_collection = NULL,
  scheduler = NULL
) {
  prelocal_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    garbage_collection = garbage_collection,
    scheduler = scheduler
  )
}

#' @title Abstract class for prelocal algorithm objects.
#' @aliases tar_prelocal
#' @export
#' @keywords internal
#' @description Not a user-side R6 class.
#'   Used to build external HPC scheduling algorithms.
prelocal_class <- R6::R6Class(
  classname = "tar_prelocal",
  inherit = local_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Initialize a prelocal algorithm object.
    #' @param pipeline Pipeline object.
    #' @param meta Meta object.
    #' @param names Character, names of targets.
    #' @param queue Character, type of queue.
    #' @param reporter Character, type of reporter.
    #' @param garbage_collection Logical, whether to
    #'   periodically run garbage collection.
    #' @param scheduler Scheduler object.
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      garbage_collection = NULL,
      scheduler = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter,
        garbage_collection = garbage_collection
      )
      self$scheduler <- scheduler
    },
    #' @description Start the algorithm.
    start = function() {
    },
    #' @description End the algorithm.
    end = function() {
    },
    #' @description Assert that the target runs locally
    #'   and exit otherwise. The special error condition defers
    #'   control to the calling algorithm.
    #' @param target Target object.
    assert_deployment = function(target) {
      should_abort <- target$settings$deployment == "remote" &&
        inherits(target, "tar_builder")
      if (should_abort) {
        name <- target_get_name(target)
        rank <- rank_offset(target$settings$priority)
        self$scheduler$queue$enqueue(name, rank)
        throw_prelocal("requires remote workers")
      }
    }
  )
)
