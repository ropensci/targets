sitrep_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  queue = "sequential",
  reporter = "silent"
) {
  sitrep_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter
  )
}

sitrep_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  queue = NULL,
  reporter = NULL
) {
  sitrep_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    queue = queue,
    reporter = reporter
  )
}

sitrep_class <- R6::R6Class(
  classname = "tar_sitrep",
  inherit = algorithm_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    checked = NULL,
    sitrep = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      queue = NULL,
      reporter = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        queue = queue,
        reporter = reporter
      )
      self$sitrep <- new.env(parent = emptyenv())
    },
    has_children = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      length(target_get_children(target)) > 0L
    },
    process_pattern = function(target) {
      if (all(map_lgl(target$settings$dimensions, self$has_children))) {
        target_skip(target, self$pipeline, self$scheduler, self$meta)
      }
    },
    process_builder = function(target) {
      name <- target_get_name(target)
      target <- pipeline_get_target(self$pipeline, name)
      target_update_depend(target, meta)
      self$sitrep[[name]] <- builder_sitrep(target, self$meta)
      trn(
        self$meta$exists_record(target_get_name(target)),
        target_skip(target, self$pipeline, self$scheduler, self$meta),
        target_update_queue(target, self$scheduler)
      )
    },
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      trn(
        inherits(target, "tar_pattern"),
        self$process_pattern(target),
        self$process_builder(target)
      )
    },
    ensure_meta = function() {
      self$meta$database$ensure_preprocessed(write = FALSE)
      envir <- pipeline_get_envir(self$pipeline)
      self$meta$set_imports(envir, self$pipeline)
    },
    start = function() {
      pipeline_prune_names(self$pipeline, self$names)
      pipeline_reset_priorities(self$pipeline)
      self$update_scheduler()
      self$ensure_meta()
      self$scheduler$reporter$report_start()
    },
    end = function() {
      self$scheduler$reporter$report_end()
    },
    run = function() {
      self$start()
      queue <- self$scheduler$queue
      while (queue$should_dequeue()) {
        self$process_target(self$scheduler$queue$dequeue())
      }
      self$end()
      invisible()
    }
  )
)
