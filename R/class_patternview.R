patternview_init <- function(seconds = 0, bytes = 0, progress = "queued") {
  patternview_new(seconds, bytes, progress)
}

patternview_new <- function(seconds = 0, bytes = 0, progress = NULL) {
  force(seconds)
  force(bytes)
  force(progress)
  force(progress)
  environment()
}

patternview_register_meta <- function(patternview, record) {
  patternview_register_seconds(patternview, record)
  patternview_register_bytes(patternview, record)
}

patternview_register_seconds <- function(patternview, record) {
  patternview$seconds <- patternview$seconds + record$seconds
}

patternview_register_bytes <- function(patternview, record) {
  patternview$bytes <- patternview$bytes + record$bytes
}

patternview_register_started <- function(patternview, target, scheduler) {
  if (identical(patternview$progress, "queued")) {
    patternview$progress <- "started"
    scheduler$progress$enqueue_started(target)
  }
}

patternview_register_canceled <- function(patternview, target, scheduler) {
  if (!(patternview$progress %in% c("canceled", "errored"))) {
    patternview$progress <- "canceled"
    scheduler$progress$enqueue_canceled(target)
  }
}

patternview_register_errored <- function(patternview, target, scheduler) {
  if (!identical(patternview$progress, "errored")) {
    patternview$progress <- "errored"
    scheduler$progress$enqueue_errored(target)
  }
}

patternview_register_final <- function(patternview, target, scheduler) {
  if (identical(patternview$progress, "started")) {
    patternview$progress <- "built"
    scheduler$progress$enqueue_built(target)
  } else if (identical(patternview$progress, "queued")) {
    patternview$progress <- "skipped"
    scheduler$progress$enqueue_skipped(target)
  }
}

patternview_validate <- function(patternview) {
  tar_assert_correct_fields(patternview, patternview_new)
  tar_assert_in(
    patternview$progress,
    c("queued", "started", "built", "canceled", "errored")
  )
}
