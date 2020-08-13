sitrep_init <- function(
  pipeline = NULL,
  names = NULL,
  queue = "sequential",
  meta = meta_init(),
  reporter = "silent"
) {
  pipeline_prune_names(pipeline, names)
  pipeline_reset_priorities(pipeline)
  scheduler <- pipeline_produce_scheduler(pipeline, queue, reporter)
  sitrep_new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    sitrep = new.env(parent = emptyenv())
  )
}

sitrep_new <- function(
  pipeline = NULL,
  scheduler = NULL,
  meta = NULL,
  sitrep = NULL
) {
  sitrep_class$new(
    pipeline = pipeline,
    scheduler = scheduler,
    meta = meta,
    sitrep = sitrep
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
      scheduler = NULL,
      meta = NULL,
      sitrep = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        scheduler = scheduler,
        meta = meta,
        garbage_collection = FALSE
      )
      self$sitrep <- sitrep
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
