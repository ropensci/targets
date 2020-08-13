algorithm_init <- function(
  subclass = "local",
  pipeline = NULL,
  names = NULL,
  queue = "parallel",
  meta = meta_init(),
  reporter = "verbose",
  garbage_collection = FALSE,
  workers = 1L,
  template = list(),
  log_worker = FALSE
) {
  algorithm_preprocess_pipeline(subclass, pipeline, names)
  scheduler <- pipeline_produce_scheduler(pipeline, queue, reporter)
  switch(
    subclass,
    clustermq = clustermq_new(
      pipeline,
      scheduler,
      meta,
      garbage_collection,
      workers = as.integer(workers),
      template = as.list(template),
      log_worker = log_worker
    ),
    future = future_new(
      pipeline,
      scheduler,
      meta,
      garbage_collection,
      workers = as.integer(workers),
      crew = memory_init()
    ),
    outdated = outdated_new(
      pipeline = pipeline,
      scheduler = scheduler,
      meta = meta,
      garbage_collection = FALSE,
      checked = counter_init(),
      outdated = counter_init()
    ),
    sitrep = sitrep_new(
      pipeline = pipeline,
      scheduler = scheduler,
      meta = meta,
      garbage_collection = FALSE,
      sitrep = new.env(parent = emptyenv())
    ),
    throw_validate("unsupported algorithm")
  )
}

algorithm_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  garbage_collection = NULL
) {
  algorithm_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
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
    scheduler = NULL,
    meta = NULL,
    garbage_collection = NULL,
    initialize = function(
      pipeline = NULL,
      scheduler = NULL,
      meta = NULL,
      garbage_collection = NULL
    ) {
      self$pipeline <- pipeline
      self$scheduler <- scheduler
      self$meta <- meta
      self$garbage_collection <- garbage_collection
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
      if (self$garbage_collection) {
        gc()
      }
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
      assert_lgl(self$garbage_collection)
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
