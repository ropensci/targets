sitrep_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "sequential",
  reporter = "silent"
) {
  sitrep_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter
  )
}

sitrep_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = shortcut,
  queue = NULL,
  reporter = NULL
) {
  sitrep_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter
  )
}

sitrep_class <- R6::R6Class(
  classname = "tar_sitrep",
  inherit = passive_class,
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
      shortcut = NULL,
      queue = NULL,
      reporter = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        shortcut = shortcut,
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
        target_skip(
          target = target,
          pipeline = self$pipeline,
          scheduler = self$scheduler,
          meta = self$meta,
          active = FALSE
        )
      }
    },
    process_builder = function(target) {
      name <- target_get_name(target)
      target <- pipeline_get_target(self$pipeline, name)
      target_update_depend(target, self$pipeline, self$meta)
      self$sitrep[[name]] <- builder_sitrep(target, self$meta)
      if_any(
        self$meta$exists_record(target_get_name(target)),
        target_skip(
          target = target,
          pipeline = self$pipeline,
          scheduler = self$scheduler,
          meta = self$meta,
          active = FALSE
        ),
        target_update_queue(target, self$scheduler)
      )
    },
    process_target = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      if_any(
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
