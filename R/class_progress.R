progress_init <- function(
  queued = counter_init(),
  started = counter_init(),
  built = counter_init(),
  skipped = counter_init(),
  errored = counter_init(),
  warned = counter_init(),
  canceled = counter_init()
) {
  database <- database_progress()
  progress_new(
    database = database,
    queued = queued,
    started = started,
    built = built,
    skipped = skipped,
    errored = errored,
    warned = warned,
    canceled = canceled
  )
}

progress_new <- function(
  database = NULL,
  queued = NULL,
  started = NULL,
  skipped = NULL,
  built = NULL,
  errored = NULL,
  warned = NULL,
  canceled = NULL
) {
  progress_class$new(
    database = database,
    queued = queued,
    started = started,
    skipped = skipped,
    built = built,
    errored = errored,
    warned = warned,
    canceled = canceled
  )
}

progress_class <- R6::R6Class(
  classname = "tar_progress",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    database = NULL,
    queued = NULL,
    started = NULL,
    skipped = NULL,
    built = NULL,
    errored = NULL,
    warned = NULL,
    canceled = NULL,
    initialize = function(
      database = NULL,
      queued = NULL,
      started = NULL,
      skipped = NULL,
      built = NULL,
      errored = NULL,
      warned = NULL,
      canceled = NULL
    ) {
      self$database <- database
      self$queued <- queued
      self$started <- started
      self$skipped <- skipped
      self$built <- built
      self$errored <- errored
      self$warned <- warned
      self$canceled <- canceled
    },
    assign_dequeued = function(target) {
      counter_del_name(self$queued, target_get_name(target))
    },
    assign_queued = function(target) {
      counter_set_name(self$queued, target_get_name(target))
    },
    assign_skipped = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_set_name(self$skipped, name)
    },
    assign_started = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_set_name(self$started, name)
    },
    assign_built = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_del_name(self$started, name)
      counter_set_name(self$built, name)
    },
    assign_canceled = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$started, name)
      counter_set_name(self$canceled, name)
    },
    assign_errored = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$started, name)
      counter_set_name(self$errored, name)
    },
    assign_warned = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$started, name)
      counter_set_name(self$warned, name)
    },
    write_progress = function(target, progress) {
      db <- self$database
      name <- target_get_name(target)
      type <- target_get_type(target)
      branches <- if_any(
        identical(type, "stem"),
        0L,
        length(omit_na(target_get_children(target)))
      )
      row <- list(
        name = name,
        type = type,
        parent = target_get_parent(target),
        branches = branches,
        progress = progress
      )
      db$write_row(row)
    },
    write_started = function(target) {
      self$write_progress(target, progress = "started")
    },
    write_built = function(target) {
      self$write_progress(target, progress = "built")
    },
    write_errored = function(target) {
      self$write_progress(target, progress = "errored")
    },
    write_canceled = function(target) {
      self$write_progress(target, progress = "canceled")
    },
    register_started = function(target) {
      self$assign_started(target)
      self$write_started(target)
    },
    register_built = function(target) {
      self$assign_built(target)
      self$write_built(target)
    },
    register_errored = function(target) {
      self$assign_errored(target)
      self$write_errored(target)
    },
    register_canceled = function(target) {
      self$assign_canceled(target)
      self$write_canceled(target)
    },
    uptodate = function() {
      self$skipped$count > 0L &&
        self$built$count == 0L &&
        self$errored$count == 0L &&
        self$canceled$count == 0L
    },
    cli_end = function(time_stamp = FALSE) {
      if_any(
        self$uptodate(),
        cli_uptodate(time_stamp = time_stamp),
        cli_done(time_stamp = time_stamp)
      )
    },
    any_remaining = function() {
      self$queued$count > 0L || self$started$count > 0L
    },
    cli_data = function() {
      data_frame(
        queue = self$queued$count,
        skip = self$skipped$count,
        start = self$started$count,
        built = self$built$count,
        error = self$errored$count,
        warn = self$warned$count,
        cancel = self$canceled$count,
        time = time_stamp_short()
      )
    },
    validate = function() {
      counter_validate(self$queued)
      counter_validate(self$started)
      counter_validate(self$built)
      counter_validate(self$skipped)
      counter_validate(self$errored)
      counter_validate(self$canceled)
    }
  )
)

database_progress <- function() {
  database_init(
    path = path_progress(),
    header = header_progress(),
  )
}

header_progress <- function() {
  c("name", "type", "parent", "branches", "progress")
}
