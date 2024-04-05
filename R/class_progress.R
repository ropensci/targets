progress_init <- function(
  path_store = path_store_default(),
  queued = counter_init(),
  dispatched = counter_init(),
  completed = counter_init(),
  skipped = counter_init(),
  errored = counter_init(),
  warned = counter_init(),
  canceled = counter_init()
) {
  database <- database_progress(path_store = path_store)
  progress_new(
    database = database,
    queued = queued,
    dispatched = dispatched,
    completed = completed,
    skipped = skipped,
    errored = errored,
    warned = warned,
    canceled = canceled
  )
}

progress_new <- function(
  database = NULL,
  queued = NULL,
  dispatched = NULL,
  skipped = NULL,
  completed = NULL,
  errored = NULL,
  warned = NULL,
  canceled = NULL
) {
  progress_class$new(
    database = database,
    queued = queued,
    dispatched = dispatched,
    skipped = skipped,
    completed = completed,
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
    dispatched = NULL,
    skipped = NULL,
    completed = NULL,
    errored = NULL,
    warned = NULL,
    canceled = NULL,
    initialize = function(
      database = NULL,
      queued = NULL,
      dispatched = NULL,
      skipped = NULL,
      completed = NULL,
      errored = NULL,
      warned = NULL,
      canceled = NULL
    ) {
      self$database <- database
      self$queued <- queued
      self$dispatched <- dispatched
      self$skipped <- skipped
      self$completed <- completed
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
    assign_dispatched = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_set_name(self$dispatched, name)
    },
    assign_completed = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$queued, name)
      counter_del_name(self$dispatched, name)
      counter_set_name(self$completed, name)
    },
    assign_canceled = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$dispatched, name)
      counter_set_name(self$canceled, name)
    },
    assign_errored = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$dispatched, name)
      counter_set_name(self$errored, name)
    },
    assign_warned = function(target) {
      name <- target_get_name(target)
      counter_del_name(self$dispatched, name)
      counter_set_name(self$warned, name)
    },
    produce_row = function(target, progress) {
      name <- target_get_name(target)
      type <- target_get_type(target)
      branches <- if_any(
        identical(type, "stem"),
        0L,
        length(omit_na(target_get_children(target)))
      )
      list(
        name = name,
        type = type,
        parent = target_get_parent(target),
        branches = branches,
        progress = progress
      )
    },
    buffer_progress = function(target, progress) {
      self$database$buffer_row(self$produce_row(target, progress))
    },
    buffer_skipped = function(target) {
      self$buffer_progress(target, progress = "skipped")
    },
    buffer_dispatched = function(target) {
      self$buffer_progress(target, progress = "dispatched")
    },
    buffer_completed = function(target) {
      self$buffer_progress(target, progress = "completed")
    },
    buffer_errored = function(target) {
      self$buffer_progress(target, progress = "errored")
    },
    buffer_canceled = function(target) {
      self$buffer_progress(target, progress = "canceled")
    },
    register_skipped = function(target) {
      self$assign_skipped(target)
      self$buffer_skipped(target)
    },
    register_dispatched = function(target) {
      self$assign_dispatched(target)
      self$buffer_dispatched(target)
    },
    register_completed = function(target) {
      self$assign_completed(target)
      self$buffer_completed(target)
    },
    register_errored = function(target) {
      self$assign_errored(target)
      self$buffer_errored(target)
    },
    register_canceled = function(target) {
      self$assign_canceled(target)
      self$buffer_canceled(target)
    },
    uptodate = function() {
      self$skipped$count > 0L &&
        self$completed$count == 0L &&
        self$errored$count == 0L &&
        self$canceled$count == 0L
    },
    cli_end = function(time_stamp = FALSE, seconds_elapsed = NULL) {
      if (self$uptodate()) {
        cli_pipeline_uptodate(
          time_stamp = time_stamp,
          seconds_elapsed = seconds_elapsed
        )
      } else if (!self$any_targets()) {
        cli_pipeline_empty(
          time_stamp = time_stamp,
          seconds_elapsed = seconds_elapsed
        )
      } else if (self$errored$count > 0L) {
        cli_pipeline_errored(
          time_stamp = time_stamp,
          seconds_elapsed = seconds_elapsed
        )
      } else {
        cli_pipeline_done(
          time_stamp = time_stamp,
          seconds_elapsed = seconds_elapsed
        )
      }
    },
    any_remaining = function() {
      self$queued$count > 0L || self$dispatched$count > 0L
    },
    any_targets = function() {
      count <- self$dispatched$count +
        self$skipped$count +
        self$completed$count +
        self$errored$count +
        self$warned$count +
        self$canceled$count
      count > 0L
    },
    cli_data = function() {
      list(
        queued = self$queued$count,
        skipped = self$skipped$count,
        dispatched = self$dispatched$count,
        completed = self$completed$count,
        errored = self$errored$count,
        warned = self$warned$count,
        canceled = self$canceled$count,
        time = time_stamp_short()
      )
    },
    abridge = function() {
      counter_del_names(self$queued, counter_get_names(self$queued))
    },
    validate = function() {
      counter_validate(self$queued)
      counter_validate(self$dispatched)
      counter_validate(self$completed)
      counter_validate(self$skipped)
      counter_validate(self$errored)
      counter_validate(self$canceled)
    }
  )
)

database_progress <- function(path_store) {
  database_init(
    path = path_progress(path_store = path_store),
    subkey = file.path(basename(path_meta("")), "progress"),
    header = header_progress(),
    integer_columns = "branches"
  )
}

header_progress <- function() {
  c("name", "type", "parent", "branches", "progress")
}
