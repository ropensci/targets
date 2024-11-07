metrics_new <- function(
  seconds = NULL,
  warnings = NULL,
  error = NULL,
  error_class = NULL,
  traceback = NULL,
  cancel = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$seconds <- seconds
  out$warnings <- warnings
  out$error <- error
  out$error_class <- error_class
  out$traceback <- traceback
  out$cancel <- cancel
  out
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
  if (metrics_has_cancel(metrics)) {
    return("cancel")
  }
  if (metrics_has_error(metrics)) {
    return("error")
  }
  "completed"
}

metrics_validate <- function(metrics) {
  tar_assert_correct_fields(metrics, metrics_new)
  tar_assert_dbl(metrics$seconds)
  tar_assert_scalar(metrics$seconds)
  tar_assert_chr(metrics$warnings %|||% character(0))
  tar_assert_chr(metrics$error %|||% character(0))
  tar_assert_chr(metrics$error_class %|||% character(0))
  tar_assert_chr(metrics$traceback %|||% character(0))
  tar_assert_lgl(metrics$cancel %|||% FALSE)
  invisible()
}
