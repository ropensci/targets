sitrep_init <- function(seconds = 0, bytes = 0, progress = "queued") {
  sitrep_new(seconds, bytes, progress)
}

sitrep_new <- function(seconds = 0, bytes = 0, progress = NULL) {
  force(seconds)
  force(bytes)
  force(progress)
  force(progress)
  environment()
}

sitrep_register_meta <- function(sitrep, record) {
  sitrep_register_seconds(sitrep, record)
  sitrep_register_bytes(sitrep, record)
}

sitrep_register_seconds <- function(sitrep, record) {
  sitrep$seconds <- sitrep$seconds + record$seconds
}

sitrep_register_bytes <- function(sitrep, record) {
  sitrep$bytes <- sitrep$bytes + record$bytes
}

sitrep_register_running <- function(sitrep, target, scheduler) {
  sitrep$progress <- "running"
  scheduler$progress$write_running(target_get_name(target))
}

sitrep_register_cancelled <- function(sitrep, target, scheduler) {
  if (sitrep$progress != "errored") {
    sitrep$progress <- "cancelled"
    scheduler$progress$write_cancelled(target_get_name(target))
  }
}

sitrep_register_errored <- function(sitrep, target, scheduler) {
  sitrep$progress <- "errored"
  scheduler$progress$write_errored(target_get_name(target))
}

sitrep_register_built <- function(sitrep, target, scheduler) {
  sitrep$progress <- "built"
}

sitrep_register_final <- function(sitrep, target, scheduler) {
  if (sitrep$progress == "running") {
    sitrep_register_built(sitrep, target, scheduler)
    scheduler$progress$write_built(target_get_name(target))
  }
}

sitrep_validate <- function(sitrep) {
  assert_correct_fields(sitrep, sitrep_new)
  assert_in(
    sitrep$progress,
    c("queued", "running", "built", "cancelled", "errored")
  )
}
