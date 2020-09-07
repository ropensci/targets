algorithm_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL,
  garbage_collection = NULL
) {
  algorithm_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter,
    garbage_collection = garbage_collection
  )
}

algorithm_class <- R6::R6Class(
  classname = "tar_algorithm",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    meta = NULL,
    scheduler = NULL,
    names = NULL,
    queue = NULL,
    reporter = NULL,
    garbage_collection = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL,
      garbage_collection = NULL
    ) {
      self$pipeline <- pipeline
      self$meta <- meta
      self$names <- names
      self$queue <- queue
      self$reporter <- reporter
      self$garbage_collection <- garbage_collection
    },
    ensure_meta = function() {
      self$meta$database$preprocess(write = TRUE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$record_imports(envir, self$pipeline)
    },
    update_scheduler = function() {
      self$scheduler <- pipeline_produce_scheduler(
        self$pipeline,
        self$queue,
        self$reporter
      )
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
      store_del_scratch()
      run_gc(self$garbage_collection)
    },
    validate = function() {
      pipeline_validate(self$pipeline)
      (self$scheduler %||% scheduler_init())$validate()
      self$meta$validate()
      assert_chr(self$names %||% character(0))
      assert_chr(self$queue %||% character(0))
      assert_chr(self$reporter %||% character(0))
      assert_lgl(self$garbage_collection)
    }
  )
)
