#' @title Conditions
#' @name tar_condition
#' @family utilities to extend targets
#' @description These functions throw custom
#'   `targets`-specific error conditions.
#'   Useful for error handling in packages built on top of `targets`.
#' @inheritParams base::stop
#' @param message Character of length 1, text of the message.
#' @param class Character vector of S3 classes of the message.
#' @examples
#' try(tar_throw_validate("something is not valid"))
NULL

#' @export
#' @rdname tar_condition
tar_message_run <- function(...) {
  tar_message(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

tar_throw_cancel <- function(...) {
  tar_error(
    message = paste0(...),
    class = c("tar_condition_cancel", "tar_condition_targets")
  )
}

tar_throw_expire <- function(...) {
  tar_error(
    message = paste0(...),
    class = c("tar_condition_expire", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_throw_file <- function(...) {
  tar_error(
    message = paste0(...),
    class = c("tar_condition_file", "tar_condition_targets")
  )
}

tar_throw_prelocal <- function(...) {
  tar_error(
    message = paste0(...),
    class = c("tar_condition_prelocal", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_throw_run <- function(..., class = character(0)) {
  tar_error(
    message = paste0(...),
    class = base::union(
      custom_error_classes(class),
      c("tar_condition_run", "tar_condition_targets")
    )
  )
}

#' @export
#' @rdname tar_condition
tar_throw_validate <- function(...) {
  tar_error(
    message = paste0(...),
    class = c("tar_condition_validate", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_warn_deprecate <- function(...) {
  tar_warning(
    message = paste0(...),
    class = c("tar_condition_deprecate", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_warn_run <- function(...) {
  tar_warning(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_warn_validate <- function(...) {
  tar_warning(
    message = paste0(...),
    class = c("tar_condition_validate", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_error <- function(message, class) {
  old_cli_number_ansi_colors <- getOption("cli.num_colors")
  on.exit(options(cli.num_colors = old_cli_number_ansi_colors))
  options(cli.num_colors = cli_number_ansi_colors)
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old))
  message <- cli::col_red(message)
  rlang::abort(message = message, class = class, call = tar_empty_envir)
}

#' @export
#' @rdname tar_condition
tar_warning <- function(message, class) {
  old_cli_number_ansi_colors <- getOption("cli.num_colors")
  on.exit(options(cli.num_colors = old_cli_number_ansi_colors))
  options(cli.num_colors = cli_number_ansi_colors)
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old))
  message <- cli::col_red(message)
  rlang::warn(message = message, class = class)
}

#' @export
#' @rdname tar_condition
tar_message <- function(message, class) {
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old))
  rlang::inform(message = message, class = class)
}

as_immediate_condition <- function(x) {
  x$call <- NULL
  enclass(x, "immediateCondition")
}

custom_error_classes <- function(class) {
  setdiff(class, default_error_classes)
}

default_error_classes <- tryCatch(
  rlang::abort("msg", class = NULL),
  error = function(condition) class(condition)
)
