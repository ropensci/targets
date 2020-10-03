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

patternview_register_running <- function(patternview, target, scheduler) {
  patternview$progress <- "running"
  scheduler$progress$write_running(target_get_name(target))
}

patternview_register_cancelled <- function(patternview, target, scheduler) {
  if (!identical(patternview$progress, "errored")) {
    patternview$progress <- "cancelled"
    scheduler$progress$write_cancelled(target_get_name(target))
  }
}

patternview_register_errored <- function(patternview, target, scheduler) {
  patternview$progress <- "errored"
  scheduler$progress$write_errored(target_get_name(target))
}

patternview_register_built <- function(patternview, target, scheduler) {
  patternview$progress <- "built"
}

patternview_register_final <- function(patternview, target, scheduler) {
  if (identical(patternview$progress, "running")) {
    patternview_register_built(patternview, target, scheduler)
    scheduler$progress$write_built(target_get_name(target))
  }
}

patternview_validate <- function(patternview) {
  assert_correct_fields(patternview, patternview_new)
  assert_in(
    patternview$progress,
    c("queued", "running", "built", "cancelled", "errored")
  )
}
