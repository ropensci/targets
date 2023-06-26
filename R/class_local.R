local_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
  seconds_interval = 0.5,
  envir = tar_option_get("envir")
) {
  local_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_interval = seconds_interval,
    garbage_collection = FALSE,
    envir = envir
  )
}

local_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_interval = NULL,
  garbage_collection = NULL,
  envir = NULL
) {
  local_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_interval = seconds_interval,
    garbage_collection = garbage_collection,
    envir = envir
  )
}

local_class <- R6::R6Class(
  classname = "tar_local",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    tar_assert_deployment = function(target) {
    },
    run_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      self$tar_assert_deployment(target)
      target_prepare(target, self$pipeline, self$scheduler, self$meta)
      target_run(
        target = target,
        envir = self$envir,
        path_store = self$meta$store
      )
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      self$unload_transient()
    },
    process_next = function() {
      self$poll_meta()
      self$process_target(self$scheduler$queue$dequeue())
    },
    run = function() {
      self$start()
      on.exit(self$end())
      queue <- self$scheduler$queue
      while (queue$should_dequeue()) {
        self$process_next()
      }
      invisible()
    }
  )
)
