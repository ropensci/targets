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
tar_message_validate <- function(...) {
  tar_message(
    message = paste0(...),
    class = c("tar_condition_validate", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_print <- function(...) {
  tar_message(
    message = paste0(...),
    class = c("tar_condition_run", "tar_condition_targets")
  )
}

#' @export
#' @rdname tar_condition
tar_error <- function(message, class) {
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old), add = TRUE)
  message <- cli::col_red(message)
  class <- safe_condition_class(class)
  rlang::abort(
    message = message,
    class = class,
    call = tar_envir_base
  )
}

#' @export
#' @rdname tar_condition
tar_warning <- function(message, class) {
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old), add = TRUE)
  message <- cli::col_red(message)
  class <- safe_condition_class(class)
  rlang::warn(message = message, class = class)
}

#' @export
#' @rdname tar_condition
tar_message <- function(message, class) {
  old <- options(rlang_backtrace_on_error = "none")
  on.exit(options(old))
  class <- safe_condition_class(class)
  rlang::inform(message = message, class = class)
}

#' @title Contain an error condition and formatted traceback.
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @return Contain an error condition and formatted traceback.
#' @param condition An error condition object thrown by `stop()` or
#'   `rlang::abort()`.
#' @param trace A raw return value from `.traceback()`.
tar_condition_traced <- function(condition, trace) {
  trace <- if_any(
    is.null(tar_runtime$traceback),
    tar_format_trace(rev(trace)),
    tar_runtime$traceback
  )
  structure(
    list(condition = condition, trace = trace),
    class = c("tar_condition_traced", "tar_condition_targets")
  )
}

tar_format_trace <- function(trace) {
  trace <- map_chr(
    trace,
    ~ {
      line <- .x
      if (!is.character(line)) {
        line <- tar_deparse_safe(.x, collapse = " ")
      }
      paste(trimws(line), collapse = " ")
    }
  )
  width <- min(getOption("width"), 79L) - 4L
  long <- nchar(trace) > width
  trace[long] <- substr(trace[long], start = 0, stop = width - 3L)
  trace[long] <- paste0(trace[long], "...")
  trace
}

as_immediate_condition <- function(x) {
  x$call <- NULL
  enclass(x, immediate_condition_s3_class)
}

immediate_condition_s3_class <- "immediateCondition"

custom_error_classes <- function(class) {
  setdiff(class, default_error_classes)
}

default_error_classes <- tryCatch(
  rlang::abort("msg", class = NULL, call = tar_envir_base),
  error = function(condition) class(condition)
)

safe_condition_class <- function(class) {
  while (length(class) && is_unsafe_condition_class(class)) {
    class <- class[-1L] # nocov
  }
  class
}

is_unsafe_condition_class <- function(class) {
  condition <- try(
    suppressMessages(rlang::inform(message = "x", class = class)),
    silent = TRUE
  )
  inherits(condition, "try-error")
}
