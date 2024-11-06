local_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "parallel",
  reporter = "verbose",
  seconds_meta_append = 0,
  seconds_meta_upload = 15,
  seconds_reporter = 0,
  envir = tar_option_get("envir")
) {
  local_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
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
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  seconds_reporter = NULL,
  envir = NULL
) {
  local_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
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
    run_target = function(target) {
      self$tar_assert_deployment(target)
      target_prepare(target, self$pipeline, self$scheduler, self$meta)
      self$sync_meta_time()
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
      self$sync_meta_time()
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
