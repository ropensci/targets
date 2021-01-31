progress_init <- function(
  queued = counter_init(),
  running = counter_init(),
  built = counter_init(),
  skipped = counter_init(),
  canceled = counter_init(),
  errored = counter_init(),
  warned = counter_init()
) {
  database <- database_progress()
  progress_new(
    database = database,
    queued = queued,
    running = running,
    built = built,
    skipped = skipped,
    canceled = canceled,
    errored = errored,
    warned = warned
  )
}

progress_new <- function(
  database = NULL,
  queued = NULL,
  running = NULL,
  skipped = NULL,
  built = NULL,
  canceled = NULL,
  errored = NULL,
  warned = NULL
) {
  progress_class$new(
    database = database,
    queued = queued,
    running = running,
    skipped = skipped,
    built = built,
    canceled = canceled,
    errored = errored,
    warned = warned
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
    running = NULL,
    skipped = NULL,
    built = NULL,
    canceled = NULL,
    errored = NULL,
    warned = NULL,
    initialize = function(
      database = NULL,
      queued = NULL,
      running = NULL,
      skipped = NULL,
      built = NULL,
      canceled = NULL,
      errored = NULL,
      warned = NULL
    ) {
      self$database <- database
      self$queued <- queued
      self$running <- running
      self$skipped <- skipped
      self$built <- built
      self$canceled <- canceled
      self$errored <- errored
      self$warned <- warned
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
    assign_running = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_set_name(self$running, name)
    },
    assign_built = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_del_name(self$running, name)
      counter_set_name(self$built, name)
    },
    assign_canceled = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$running, name)
      counter_set_name(self$canceled, name)
    },
    assign_errored = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$running, name)
      counter_set_name(self$errored, name)
    },
    assign_warned = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$running, name)
      counter_set_name(self$warned, name)
    },
    write_progress = function(target, progress) {
      db <- self$database
      name <- target_get_name(target)
      type <- target_get_type(target)
      branches <- trn(
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
    write_running = function(target) {
      self$write_progress(target, progress = "running")
    },
    write_built = function(target) {
      self$write_progress(target, progress = "built")
    },
    write_canceled = function(target) {
      self$write_progress(target, progress = "canceled")
    },
    write_errored = function(target) {
      self$write_progress(target, progress = "errored")
    },
    register_running = function(target) {
      self$assign_running(target)
      self$write_running(target)
    },
    register_built = function(target) {
      self$assign_built(target)
      self$write_built(target)
    },
    register_canceled = function(target) {
      self$assign_canceled(target)
      self$write_canceled(target)
    },
    register_errored = function(target) {
      self$assign_errored(target)
      self$write_errored(target)
    },
    uptodate = function() {
      self$skipped$count > 0L &&
        self$built$count == 0L &&
        self$canceled$count == 0L &&
        self$errored$count == 0L
    },
    cli_end = function(time_stamp = FALSE) {
      trn(
        self$uptodate(),
        cli_uptodate(time_stamp = time_stamp),
        cli_done(time_stamp = time_stamp)
      )
    },
    any_remaining = function() {
      self$queued$count > 0L || self$running$count > 0L
    },
    update_cli = function() {
      cli_progress(
        queued = self$queued$count,
        skipped = self$skipped$count,
        running = self$running$count,
        built = self$built$count,
        canceled = self$canceled$count,
        errored = self$errored$count,
        warned = self$warned$count
      )
    },
    validate = function() {
      counter_validate(self$queued)
      counter_validate(self$running)
      counter_validate(self$built)
      counter_validate(self$skipped)
      counter_validate(self$canceled)
      counter_validate(self$errored)
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
