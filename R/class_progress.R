progress_init <- function(
  path_store = path_store_default(),
  queued = counter_init(),
  dispatched = counter_init(),
  completed = counter_init(),
  skipped = counter_init(),
  errored = counter_init(),
  warned = counter_init(),
  canceled = counter_init(),
  trimmed = counter_init()
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
    canceled = canceled,
    trimmed = trimmed
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
  canceled = NULL,
  trimmed = NULL
) {
  progress_class$new(
    database = database,
    queued = queued,
    dispatched = dispatched,
    skipped = skipped,
    completed = completed,
    errored = errored,
    warned = warned,
    canceled = canceled,
    trimmed = trimmed
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
    trimmed = NULL,
    initialize = function(
      database = NULL,
      queued = NULL,
      dispatched = NULL,
      skipped = NULL,
      completed = NULL,
      errored = NULL,
      warned = NULL,
      canceled = NULL,
      trimmed = NULL
    ) {
      self$database <- database
      self$queued <- queued
      self$dispatched <- dispatched
      self$skipped <- skipped
      self$completed <- completed
      self$errored <- errored
      self$warned <- warned
      self$canceled <- canceled
      self$trimmed <- trimmed
    },
    assign_dequeued = function(name) {
      counter_del_name(self$queued, name)
    },
    assign_queued = function(name) {
      counter_set_name(self$queued, name)
    },
    assign_skipped = function(name) {
      counter_del_name(self$queued, name)
      counter_set_name(self$skipped, name)
    },
    assign_dispatched = function(name) {
      counter_del_name(self$queued, name)
      counter_set_name(self$dispatched, name)
    },
    assign_completed = function(name) {
      counter_del_name(self$queued, name)
      counter_del_name(self$dispatched, name)
      counter_set_name(self$completed, name)
    },
    assign_canceled = function(name) {
      counter_del_name(self$dispatched, name)
      counter_set_name(self$canceled, name)
    },
    assign_errored = function(name) {
      counter_del_name(self$dispatched, name)
      counter_set_name(self$errored, name)
    },
    assign_warned = function(name) {
      counter_del_name(self$dispatched, name)
      counter_set_name(self$warned, name)
    },
    assign_trimmed = function(names) {
      counter_del_names(self$queued, names)
      counter_set_names(self$trimmed, names)
    },
    produce_row = function(target, progress) {
      name <- target_get_name(target)
      type <- target_get_type(target)
      if (type == "pattern") {
        branches <- length(omit_na(target_get_children(target)))
      } else {
        branches <- 0L
      }
      list(
        name = name,
        type = type,
        parent = target_get_parent(target),
        branches = branches,
        progress = progress
      )
    },
    buffer_progress = function(target, progress) {
      database <- .subset2(self, "database")
      row <- .subset2(self, "produce_row")(target, progress)
      .subset2(database, "buffer_row")(row, fill_missing = FALSE)
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
      self$assign_skipped(target_get_name(target))
      self$buffer_skipped(target)
    },
    register_dispatched = function(target) {
      self$assign_dispatched(target_get_name(target))
      self$buffer_dispatched(target)
    },
    register_completed = function(target) {
      self$assign_completed(target_get_name(target))
      self$buffer_completed(target)
    },
    register_errored = function(target) {
      self$assign_errored(target_get_name(target))
      self$buffer_errored(target)
    },
    register_canceled = function(target) {
      self$assign_canceled(target_get_name(target))
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
    count_unfinished = function(names) {
      counter_count_exists(queued, names) +
        counter_count_exists(dispatched, names)
    },
    validate = function() {
      counter_validate(self$queued)
      counter_validate(self$dispatched)
      counter_validate(self$completed)
      counter_validate(self$skipped)
      counter_validate(self$errored)
      counter_validate(self$canceled)
      counter_validate(self$trimmed)
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
