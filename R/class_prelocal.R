prelocal_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_interval = NULL,
  garbage_collection = NULL,
  envir = NULL,
  scheduler = NULL
) {
  prelocal_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    seconds_interval = seconds_interval,
    garbage_collection = garbage_collection,
    envir = envir,
    scheduler = scheduler
  )
}

prelocal_class <- R6::R6Class(
  classname = "tar_prelocal",
  inherit = local_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      seconds_interval = NULL,
      garbage_collection = NULL,
      envir = NULL,
      scheduler = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter,
        seconds_interval = seconds_interval,
        garbage_collection = garbage_collection,
        envir = envir
      )
      self$scheduler <- scheduler
    },
    start = function() {
    },
    end = function() {
    },
    tar_assert_deployment = function(target) {
      should_abort <- identical(target$settings$deployment, "worker") &&
        inherits(target, "tar_builder")
      if (should_abort) {
        name <- target_get_name(target)
        rank <- rank_offset(target$settings$priority)
        self$scheduler$queue$prepend(name, rank)
        tar_throw_prelocal("requires workers")
      }
    }
  )
)
