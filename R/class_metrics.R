metrics_new <- function(
  seconds = NULL,
  warnings = NULL,
  error = NULL,
  traceback = NULL,
  cancel = NULL
) {
  force(seconds)
  force(warnings)
  force(error)
  force(traceback)
  force(cancel)
  environment()
}

metrics_has_warnings <- function(metrics) {
  !is.null(metrics$warnings)
}

metrics_has_error <- function(metrics) {
  !is.null(metrics$error)
}

metrics_has_cancel <- function(metrics) {
  !is.null(metrics$cancel)
}

metrics_terminated_early <- function(metrics) {
  metrics_has_error(metrics) || metrics_has_cancel(metrics)
}

metrics_outcome <- function(metrics) {
  trn(
    metrics_has_cancel(metrics),
    "cancel",
    trn(metrics_has_error(metrics), "error", "built")
  )
}

metrics_validate <- function(metrics) {
  assert_correct_fields(metrics, metrics_new)
  assert_dbl(metrics$seconds)
  assert_scalar(metrics$seconds)
  assert_chr(metrics$warnings %||% character(0))
  assert_chr(metrics$error %||% character(0))
  assert_chr(metrics$traceback %||% character(0))
  assert_lgl(metrics$cancel %||% FALSE)
  invisible()
}
