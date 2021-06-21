as_immediate_condition <- function(x) {
  x$call <- NULL
  enclass(x, "immediateCondition")
}

tar_message_run <- function(...) {
  rlang::inform(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

tar_throw_cancel <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("tar_condition_cancel", "tar_condition_targets")
  )
}

tar_throw_file <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("tar_condition_file", "tar_condition_targets")
  )
}

tar_throw_prelocal <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("tar_condition_prelocal", "tar_condition_targets")
  )
}

tar_throw_run <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

tar_throw_validate <- function(...) {
  rlang::abort(
    message = paste0(...),
    class = c("tar_condition_validate", "tar_condition_targets")
  )
}

tar_warn_deprecate <- function(...) {
  rlang::warn(
    message = paste0(...),
    class = c("tar_condition_deprecate", "tar_condition_targets")
  )
}

tar_warn_run <- function(...) {
  rlang::warn(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

tar_warn_validate <- function(...) {
  rlang::warn(
    message = paste0(...),
    class = c("tar_condition_validate", "tar_condition_targets")
  )
}
