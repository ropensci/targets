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

active_class <- R6::R6Class(
  classname = "tar_active",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    garbage_collection = NULL,
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
    ensure_meta = function() {
      self$meta$validate()
      self$meta$database$preprocess(write = TRUE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$record_imports(envir, self$pipeline)
    },
    run_gc = function() {
      if (self$garbage_collection) {
        gc()
      }
    },
    unload_transient = function() {
      pipeline_unload_transient(self$pipeline)
    },
    unserialize_target = function(target) {
      builder_unserialize_value(target)
    },
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      target_debug(target)
      target_update_depend(target, meta)
      trn(
        target_should_run(target, self$meta),
        self$run_target(name),
        self$skip_target(target)
      )
      store_sync_file_meta(target$store, target, self$meta)
    },
    start = function() {
      pipeline_prune_names(self$pipeline, self$names)
      self$update_scheduler()
      self$ensure_meta()
      self$scheduler$progress$database$reset_storage()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      pipeline_unload_loaded(self$pipeline)
      scheduler <- self$scheduler
      scheduler$reporter$report_end(scheduler$progress)
      path_scratch_del()
      self$run_gc()
    },
    validate = function() {
      super$validate()
      assert_lgl(self$garbage_collection)
    }
  )
)
