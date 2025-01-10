outdated_init <- function(
  pipeline = NULL,
  meta = meta_init(),
  names = NULL,
  shortcut = FALSE,
  queue = "sequential",
  reporter = "silent",
  seconds_meta_append = 0,
  seconds_meta_upload = 15,
  seconds_reporter = 0.5
) {
  outdated_new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    checked = counter_init(),
    outdated = counter_init()
  )
}

outdated_new <- function(
  pipeline = NULL,
  meta = NULL,
  names = NULL,
  shortcut = NULL,
  queue = NULL,
  reporter = NULL,
  seconds_meta_append = NULL,
  seconds_meta_upload = NULL,
  seconds_reporter = NULL,
  checked = NULL,
  outdated = NULL
) {
  outdated_class$new(
    pipeline = pipeline,
    meta = meta,
    names = names,
    shortcut = shortcut,
    queue = queue,
    reporter = reporter,
    seconds_meta_append = seconds_meta_append,
    seconds_meta_upload = seconds_meta_upload,
    seconds_reporter = seconds_reporter,
    checked = checked,
    outdated = outdated
  )
}

outdated_class <- R6::R6Class(
  classname = "tar_outdated",
  inherit = passive_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    checked = NULL,
    outdated = NULL,
    initialize = function(
      pipeline = NULL,
      meta = NULL,
      names = NULL,
      shortcut = NULL,
      queue = NULL,
      reporter = NULL,
      seconds_meta_append = NULL,
      seconds_meta_upload = NULL,
      seconds_reporter = NULL,
      checked = NULL,
      outdated = NULL
    ) {
      super$initialize(
        pipeline = pipeline,
        meta = meta,
        names = names,
        shortcut = shortcut,
        queue = queue,
        reporter = reporter,
        seconds_meta_append = seconds_meta_append,
        seconds_meta_upload = seconds_meta_upload,
        seconds_reporter = seconds_reporter
      )
      self$checked <- checked
      self$outdated <- outdated
    },
    # nocov start
    # Covered in tests/interactive/test-reporter.R.
    cli_data = function() {
      list(checked = self$checked$count, outdated = self$outdated$count)
    },
    # nocov end
    is_outdated = function(name) {
      counter_exists_name(self$outdated, name)
    },
    reset_hash = function(name) {
      database <- .subset2(meta, "database")
      row <- .subset2(database, "get_row")(name)
      row$data <- NA_character_
      .subset2(database, "set_row")(row)
    },
    reset_junction = function(target) {
      if (!is.null(target$junction)) {
        junction_invalidate(target$junction)
      }
    },
    register_checked = function(name) {
      counter_set_name(self$checked, name)
    },
    register_outdated = function(name) {
      counter_set_name(self$outdated, name)
      if (self$meta$exists_record(name)) {
        self$reset_hash(name)
      }
    },
    register_builder_outdated = function(target) {
      self$register_outdated(target_get_name(target))
      self$reset_junction(target)
    },
    process_builder_exists = function(target) {
      tryCatch(
        target_skip(
          target = target,
          pipeline = self$pipeline,
          scheduler = self$scheduler,
          meta = self$meta,
          active = FALSE
        ),
        error = function(e) warning(e$message)
      )
      target_update_depend(target, self$pipeline, self$meta)
      if (target_should_run(target, self$meta)) {
        self$register_builder_outdated(target)
      }
    },
    process_builder_missing = function(target) {
      target_update_queue(target, self$scheduler)
      self$register_builder_outdated(target)
    },
    process_builder = function(target) {
      if_any(
        self$meta$exists_record(target_get_name(target)),
        self$process_builder_exists(target),
        self$process_builder_missing(target)
      )
    },
    is_childless = function(name) {
      target <- pipeline_get_target(self$pipeline, name)
      !length(target_get_children(target))
    },
    process_pattern = function(target) {
      if (any(map_lgl(target$settings$dimensions, self$is_childless))) {
        self$register_outdated(target_get_name(target))
        return()
      }
      target_skip(
        target = target,
        pipeline = self$pipeline,
        scheduler = self$scheduler,
        meta = self$meta,
        active = FALSE
      )
      if (any(map_lgl(target_get_children(target), self$is_outdated))) {
        self$register_outdated(target_get_name(target))
      }
    },
    process_target = function(name, pipeline) {
      target <- pipeline_get_target(pipeline, name)
      if_any(
        inherits(target, "tar_pattern"),
        process_pattern(target),
        process_builder(target)
      )
      register_checked(name)
      scheduler$reporter$report_outdated(self)
    },
    run = function() {
      self$start()
      queue <- self$scheduler$queue
      should_dequeue <- queue$should_dequeue
      dequeue <- queue$dequeue
      pipeline <- self$pipeline
      while (should_dequeue()) {
        name <- dequeue()
        self$process_target(name, pipeline)
      }
      scheduler$reporter$report_outdated_end(self)
      self$end()
    },
    validate = function() {
      super$validate()
      counter_validate(self$outdated)
    }
  )
)
