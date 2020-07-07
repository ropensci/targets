build_init <- function(expr, envir, seed = 0L) {
  start <- build_time_seconds()
  capture_error <- function(e) {
    state$error <- paste(c(e$message, " ."), collapse = "")
    state$traceback <- as.character(sys.calls())
    NULL
  }
  capture_cancel <- function(e) {
    state$cancel <- TRUE
    NULL
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  object <- tryCatch(
    withCallingHandlers(
      withr::with_dir(getwd(), withr::with_seed(seed, eval(expr, envir))),
      error = capture_error,
      warning = function(w) {
        state$warnings <- c(state$warnings, w$message)
        warning(as_immediate_condition(w))
        invokeRestart("muffleWarning")
      },
      condition_cancel = capture_cancel
    ),
    error = function(e) {
    }
  )
  metrics <- metrics_new(
    seconds = round(build_time_seconds() - start, 3),
    warnings = state$warnings,
    error = state$error,
    traceback = state$traceback,
    cancel = state$cancel
  )
  build_new(object, metrics)
}

build_new <- function(object = NULL, metrics = NULL) {
  force(object)
  force(metrics)
  environment()
}

build_validate <- function(build) {
  assert_correct_fields(build, build_new)
  metrics_validate(build$metrics)
}

build_time_seconds <- function() {
  as.numeric(proc.time()["elapsed"])
}
