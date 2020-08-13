algorithm_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL
) {
  algorithm_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta
  )
}

algorithm_class <- R6::R6Class(
  classname = "tar_algorithm",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    pipeline = NULL,
    scheduler = NULL,
    meta = NULL,
    initialize = function(
      pipeline = NULL,
      scheduler = NULL,
      meta = NULL
    ) {
      self$pipeline <- pipeline
      self$scheduler <- scheduler
      self$meta <- meta
    },
    ensure_meta = function() {
      self$meta$database$preprocess(write = TRUE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$record_imports(envir, self$pipeline)
    },
    start_algorithm = function() {
      self$scheduler$reporter$report_start()
      self$ensure_meta()
      self$scheduler$progress$database$reset_storage()
    },
    end_algorithm = function() {
      pipeline_unload_loaded(self$pipeline)
      scheduler <- self$scheduler
      scheduler$reporter$report_end(scheduler$progress)
      store_del_scratch()
    },
    start = function() {
      self$start_algorithm()
    },
    end = function() {
      self$end_algorithm()
    },
    validate = function() {
      pipeline_validate(self$pipeline)
      self$scheduler$validate()
      self$meta$validate()
    }
  )
)

algorithm_preprocess_pipeline <- function(subclass, pipeline, names) {
  if (!is.null(names)) {
    pipeline_prune_targets(pipeline, names)
  }
  if (subclass == "outdated") {
    pipeline_reset_priorities(pipeline)
  }
}
