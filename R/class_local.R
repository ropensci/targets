local_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  queue = "parallel",
  reporter = "verbose"
) {
  local_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter
  )
}

local_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL
) {
  local_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter
  )
}

local_class <- R6::R6Class(
  classname = "tar_local",
  inherit = active_class,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    assert_deployment = function(target) {
    },
    run_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      target_gc(target)
      self$assert_deployment(target)
      target_prepare(target, self$pipeline, self$scheduler)
      target_run(target = target, envir = tar_option_get("envir"))
      target_conclude(
        target,
        self$pipeline,
        self$scheduler,
        self$meta
      )
      self$unload_transient()
    },
    process_next = function() {
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
