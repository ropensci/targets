progress_init <- function(
  queued = counter_init(),
  running = counter_init(),
  built = counter_init(),
  skipped = counter_init(),
  cancelled = counter_init(),
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
    cancelled = cancelled,
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
  cancelled = NULL,
  errored = NULL,
  warned = NULL
) {
  progress_class$new(
    database = database,
    queued = queued,
    running = running,
    skipped = skipped,
    built = built,
    cancelled = cancelled,
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
    cancelled = NULL,
    errored = NULL,
    warned = NULL,
    initialize = function(
      database = NULL,
      queued = NULL,
      running = NULL,
      skipped = NULL,
      built = NULL,
      cancelled = NULL,
      errored = NULL,
      warned = NULL
    ) {
      self$database <- database
      self$queued <- queued
      self$running <- running
      self$skipped <- skipped
      self$built <- built
      self$cancelled <- cancelled
      self$errored <- errored
      self$warned <- warned
    },
    assign_dequeued = function(names) {
      counter_del_names(self$queued, names)
    },
    assign_queued = function(names) {
      counter_set_names(self$queued, names)
    },
    assign_skipped = function(names) {
      counter_del_names(self$queued, names)
      counter_set_names(self$skipped, names)
    },
    assign_running = function(names) {
      counter_del_names(self$queued, names)
      counter_set_names(self$running, names)
    },
    assign_built = function(names) {
      counter_del_names(self$queued, names)
      counter_del_names(self$running, names)
      counter_set_names(self$built, names)
    },
    assign_cancelled = function(names) {
      counter_del_names(self$running, names)
      counter_set_names(self$cancelled, names)
    },
    assign_errored = function(names) {
      counter_del_names(self$running, names)
      counter_set_names(self$errored, names)
    },
    assign_warned = function(names) {
      counter_del_names(self$running, names)
      counter_set_names(self$warned, names)
    },
    write_running = function(names) {
      db <- self$database
      map(names, ~db$write_row(list(name = .x, progress = "running")))
    },
    write_built = function(names) {
      db <- self$database
      map(names, ~db$write_row(list(name = .x, progress = "built")))
    },
    write_cancelled = function(names) {
      db <- self$database
      map(names, ~db$write_row(list(name = .x, progress = "cancelled")))
    },
    write_errored = function(names) {
      db <- self$database
      map(names, ~db$write_row(list(name = .x, progress = "errored")))
    },
    register_running = function(names) {
      self$assign_running(names)
      self$write_running(names)
    },
    register_built = function(names) {
      self$assign_built(names)
      self$write_built(names)
    },
    register_cancelled = function(names) {
      self$assign_cancelled(names)
      self$write_cancelled(names)
    },
    register_errored = function(names) {
      self$assign_errored(names)
      self$write_errored(names)
    },
    uptodate = function() {
      self$skipped$count > 0L &&
        self$built$count == 0L &&
        self$cancelled$count == 0L &&
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
        cancelled = self$cancelled$count,
        errored = self$errored$count,
        warned = self$warned$count
      )
    },
    validate = function() {
      counter_validate(self$queued)
      counter_validate(self$running)
      counter_validate(self$built)
      counter_validate(self$skipped)
      counter_validate(self$cancelled)
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

path_progress <- function() {
  file.path("_targets", "meta", "progress")
}

header_progress <- function() {
  c("name", "progress")
}
