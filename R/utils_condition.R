msg_run <- function(...) {
  message(message_run(...))
}

warn_deprecate <- function(...) {
  warning(warning_deprecate(...))
}

warn_run <- function(...) {
  warning(warning_run(...))
}

warn_validate <- function(...) {
  warning(warning_validate(...))
}

throw_cancel <- function(...) {
  stop(error_cancel(...))
}

throw_file <- function(...) {
  stop(error_file(...))
}

throw_prelocal <- function(...) {
  stop(error_prelocal(...))
}

throw_run <- function(...) {
  stop(error_run(...))
}

throw_validate <- function(...) {
  stop(error_validate(...))
}

message_run <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_run", "condition_targets", "message", "condition")
  )
}

warning_deprecate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c(
      "condition_deprecate",
      "condition_targets",
      "warning",
      "condition"
    )
  )
}

warning_run <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_run", "condition_targets", "warning", "condition")
  )
}

warning_validate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_validate", "condition_targets", "warning", "condition")
  )
}

error_cancel <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_cancel", "condition_targets", "error", "condition")
  )
}

error_file <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_file", "condition_targets", "error", "condition")
  )
}

error_prelocal <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_prelocal", "condition_targets", "error", "condition")
  )
}

error_run <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_run", "condition_targets", "error", "condition")
  )
}

error_validate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c("condition_validate", "condition_targets", "error", "condition")
  )
}

as_immediate_condition <- function(x) {
  x$call <- NULL
  enclass(x, "immediateCondition")
}
