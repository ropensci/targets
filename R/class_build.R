build_init <- function(
  expr,
  envir,
  seed = 1L,
  packages = character(0),
  library = NULL
) {
  start <- build_time_seconds()
  capture_error <- function(condition) {
    state$error <- build_message(condition)
    state$traceback <- build_traceback(condition, sys.calls())
    NULL
  }
  capture_warning <- function(condition) {
    state$warnings <- build_message(condition)
    warning(as_immediate_condition(condition))
    invokeRestart("muffleWarning")
  }
  capture_cancel <- function(condition) {
    state$cancel <- TRUE
    NULL
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
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

# require() is faster than library() # nolint
# but we should still fail early and loudly when needed.
build_load_packages <- function(packages, library) {
  out <- suppressPackageStartupMessages(
    lapply(
      packages,
      require,
      lib.loc = library,
      quietly = TRUE,
      character.only = TRUE
    )
  )
  out <- as.logical(unlist(out))
  msg <- paste(
    "could not find packages",
    paste(packages[!out], collapse = ", "),
    "in library paths",
    paste(library, collapse = ", ")
  )
  tar_assert_true(all(out), msg)
}

build_run_expr <- function(expr, envir, seed, packages, library) {
  build_load_packages(packages, library)
  withr::with_dir(
    getwd(),
    withr::with_seed(seed, build_eval_fce17be7(expr, envir))
  )
}

# Marker to shorten tracebacks.
build_eval_fce17be7 <- function(expr, envir) {
  eval(expr = expr, envir = envir)
}

build_traceback <- function(condition, calls) {
  UseMethod("build_traceback")
}

#' @export
build_traceback.tar_condition_cancel <- function(condition, calls) {
  NULL
}

#' @export
build_traceback.default <- function(condition, calls) {
  as.character(calls)
}

build_time_seconds <- function() {
  as.numeric(proc.time()["elapsed"])
}

build_message <- function(condition, prefix = character(0)) {
  out <- substr(
    paste(c(prefix, conditionMessage(condition)), collapse = " "),
    start = 0L,
    stop = 2048L
  )
  if_any(nzchar(out), out, ".")
}

build_validate <- function(build) {
  tar_assert_correct_fields(build, build_new)
  metrics_validate(build$metrics)
}
