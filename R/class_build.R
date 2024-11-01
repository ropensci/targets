build_init <- function(
  expr,
  envir,
  seed = 0L,
  packages = character(0),
  library = NULL
) {
  capture_error <- function(condition) {
    state$error <- build_message(condition)
    state$error_class <- class(condition)
    state$traceback <- build_traceback(sys.calls())
    NULL
  }
  capture_warning <- function(condition) {
    state$count_warnings <- (state$count_warnings %||% 0L) + 1L
    should_store_warning <- (state$count_warnings < 51L) &&
      (nchar(state$warnings %||% "") < build_message_max_nchar)
    if (should_store_warning) {
      state$warnings <- paste(
        c(state$warnings, build_message(condition)),
        collapse = ". "
      )
    }
    warning(as_immediate_condition(condition))
    invokeRestart("muffleWarning")
  }
  capture_cancel <- function(condition) {
    state$cancel <- TRUE
    NULL
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  start <- time_seconds()
  object <- tryCatch(
    withCallingHandlers(
      build_run_expr(expr, envir, seed, packages, library),
      error = capture_error,
      warning = capture_warning,
      tar_condition_cancel = capture_cancel
    ),
    error = function(condition) {
    }
  )
  if (!is.null(state$warnings)) {
    state$warnings <- build_message_text_substr(state$warnings)
  }
  metrics <- metrics_new(
    seconds = round(time_seconds() - start, 3),
    warnings = state$warnings,
    error = state$error,
    error_class = state$error_class,
    traceback = state$traceback,
    cancel = state$cancel
  )
  build_new(object, metrics)
}

build_new <- function(object = NULL, metrics = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$object <- object
  out$metrics <- metrics
  out
}

build_run_expr <- function(expr, envir, seed, packages, library) {
  load_packages(packages = packages, library = library)
  tar_seed_set(seed = seed)
  build_eval_fce17be7(expr, envir)
}

# Marker to shorten tracebacks.
build_eval_fce17be7 <- function(expr, envir) {
  eval(expr = expr, envir = envir)
}

build_traceback <- function(calls) {
  trace <- tar_format_trace(calls)
  limit <- max(grep(pattern = "build_eval_fce17be7", x = trace)) + 3L
  trace[seq(from = limit, to = length(trace))]
}

build_message <- function(condition, prefix = character(0)) {
  out <- build_message_text_substr(
    message = conditionMessage(condition),
    prefix = prefix
  )
  if_any(nzchar(out), out, ".")
}

build_message_text_substr <- function(message, prefix = character(0)) {
  tryCatch(
    substr(
      paste(c(prefix, message), collapse = " "),
      start = 0L,
      stop = build_message_max_nchar
    ),
    error = function(condition) {
      paste(
        "targets could not process the error or warning message",
        "due to a text encoding issue."
      )
    }
  )
}

build_validate <- function(build) {
  tar_assert_correct_fields(build, build_new)
  metrics_validate(build$metrics)
}

build_message_max_nchar <- 2048L
