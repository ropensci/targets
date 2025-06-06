callr_outer <- function(
  targets_function,
  targets_arguments,
  callr_function,
  callr_arguments,
  envir,
  script,
  store,
  fun
) {
  tar_assert_script(script)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  choice <- tar_assert_store_noninvalidating(
    store,
    threshold = "1.8.0",
    prompt = grepl("^tar_make", fun)
  )
  # Tested in tests/interactive/test-tar_assert_store_noninvalidating.R
  # nocov start
  if (identical(choice, 1L)) {
    tar_message_run("Pipeline stopped.")
    return(invisible())
  }
  # nocov end
  out <- callr_dispatch(
    targets_function = targets_function,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = fun
  )
  if_any(
    inherits(out, "tar_condition_traced"),
    callr_error(traced_condition = out, fun = fun),
    out
  )
}

callr_error <- function(traced_condition, fun) {
  lines <- c(
    sprintf(
      "Error in %s():\n  %s",
      fun,
      conditionMessage(traced_condition$condition)
    ),
    paste(
      "  See",
      cli_url("https://books.ropensci.org/targets/debugging.html")
    )
  )
  message <- paste(lines, collapse = "\n")
  tar_throw_run(message, class = class(traced_condition$condition))
}

callr_dispatch <- function(
  targets_function,
  targets_arguments,
  callr_function,
  callr_arguments,
  envir,
  script,
  store,
  fun
) {
  in_rstudio_job <- rstudio_available(verbose = FALSE) &&
    !is.null(getNamespace("rstudioapi")[["isBackgroundJob"]]) &&
    rstudioapi::isBackgroundJob()
  options <- list(
    cli.dynamic = cli::is_dynamic_tty() || in_rstudio_job,
    cli.num_colors = cli::num_ansi_colors()
  )
  pid_parent <- as.integer(Sys.getpid())
  callr_arguments$func <- callr_inner
  callr_arguments$args <- list(
    targets_function = targets_function,
    targets_arguments = targets_arguments,
    options = options,
    script = script,
    store = store,
    fun = fun,
    pid_parent = pid_parent
  )
  if_any(
    is.null(callr_function),
    callr_inner(
      targets_function = targets_function,
      targets_arguments = targets_arguments,
      options = options,
      envir = envir,
      script = script,
      store = store,
      fun = fun,
      pid_parent = pid_parent
    ),
    do.call(
      callr_function,
      callr_prepare_arguments(callr_function, callr_arguments)
    )
  )
}

callr_inner <- function(
  targets_function,
  targets_arguments,
  options,
  envir = NULL,
  script,
  store,
  fun,
  pid_parent
) {
  force(envir)
  parent <- parent.frame()
  result <- new.env(parent = emptyenv())
  tryCatch(
    out <- withCallingHandlers(
      targets::tar_callr_inner_try(
        targets_function = targets_function,
        targets_arguments = targets_arguments,
        options = options,
        envir = envir,
        parent = parent,
        script = script,
        store = store,
        fun = fun,
        pid_parent = pid_parent
      )
      ,
      error = function(condition) {
        trace <- .traceback(x = 3L)
        result$condition <- targets::tar_condition_traced(
          condition = condition,
          trace = trace
        )
      }
    ),
    error = function(condition) {
    }
  )
  if (is.null(result$condition)) {
    out
  } else {
    result$condition
  }
}

#' @title Invoke a `targets` task from inside a `callr` function
#'   (without error handling).
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#'   Exported for internal purposes only.
#' @return The output of a call to a `targets` function that uses
#'   `callr` for reproducibility.
#' @inheritParams tar_validate
#' @param targets_function A function from `targets` to call.
#' @param targets_arguments Named list of arguments of targets_function.
#' @param options Names of global options to temporarily set
#'   in the `callr` process.
#' @param envir Name of the environment to run in. If `NULL`,
#'   the environment defaults to `tar_option_get("envir")`.
#' @param parent Parent environment of the call to
#'   `tar_call_inner()`.
#' @param fun Character of length 1, name of the `targets`
#'   function being called.
#' @param pid_parent Integer of length 1, process ID of the calling process,
#'   e.g. the one that called [tar_make()].
#' @examples
#' # See the examples of tar_make().
tar_callr_inner_try <- function(
  targets_function,
  targets_arguments,
  options,
  envir = NULL,
  parent,
  script,
  store,
  fun,
  pid_parent
) {
  old_options <- options(options)
  old_envir <- tar_options$get_envir()
  on.exit({
    # options(old_options) does not remove newly set options, only
    # the values of previously nonempty options.
    # However, a more invasive approach causes mysterious errors, including
    # false positives in compare_working_directories().
    options(old_options)
    tar_options$set_envir(old_envir)
    traceback <- tar_runtime$traceback
    runtime_reset(tar_runtime)
    tar_runtime$traceback <- traceback
  })
  callr_set_runtime(
    script = script,
    store = store,
    fun = fun,
    pid_parent = pid_parent,
    reporter = targets_arguments$reporter
  )
  envir <- if_any(is.null(envir), parent, envir)
  tar_options$set_envir(envir = envir)
  targets <- eval(parse(file = script, keep.source = TRUE), envir = envir)
  targets_arguments$pipeline <- pipeline_from_list(targets)
  pipeline_validate_lite(targets_arguments$pipeline)
  suppressPackageStartupMessages(do.call(targets_function, targets_arguments))
}

callr_set_runtime <- function(script, store, fun, pid_parent, reporter) {
  tar_runtime$script <- script
  tar_runtime$store <- store
  tar_runtime$working_directory <- getwd()
  tar_runtime$fun <- fun
  # Needs to be set here for user-side code in _targets.R outside the pipeline
  tar_runtime$active <- fun %in% c(
    "tar_make",
    "tar_make_clustermq",
    "tar_make_future"
  )
  tar_runtime$pid_parent <- pid_parent
  tar_runtime$inventories <- list()
  tar_runtime$progress_bar <- identical(as.character(reporter), "balanced")
}

callr_prepare_arguments <- function(callr_function, callr_arguments) {
  if ("show" %in% names(formals(callr_function))) {
    callr_arguments$show <- callr_arguments$show %||% TRUE
  }
  if ("env" %in% names(formals(callr_function))) {
    callr_arguments$env <- callr_arguments$env %||% callr::rcmd_safe_env()
    callr_arguments$env <- c(
      callr_arguments$env,
      PROCESSX_NOTIFY_OLD_SIGCHLD = "true"
    )
  }
  callr_arguments
}

#' @title Default `callr` arguments.
#' @export
#' @keywords internal
#' @description Default `callr` arguments for the `callr_arguments`
#'   argument of [tar_make()] and related functions.
#' @details Not a user-side function. Do not invoke directly.
#'   Exported for internal purposes only.
#' @return A list of arguments to `callr_function`.
#' @param callr_function A function from the `callr` package
#'   that starts an external R process.
#' @param reporter Character of length 1, choice of reporter
#'   for [tar_make()] or a related function.
#' @examples
#' tar_callr_args_default(callr::r)
tar_callr_args_default <- function(callr_function, reporter = NULL) {
  if (is.null(callr_function)) {
    return(list())
  }
  out <- list(spinner = FALSE, stderr = "2>&1")
  if (cli::is_dynamic_tty()) {
    out$env <- c(callr::rcmd_safe_env(), CLI_TICK_TIME = "1000") # nocov
  }
  out[intersect(names(out), names(formals(callr_function)))]
}

#' @title Deprecated: `callr` arguments.
#' @export
#' @keywords internal
#' @description Deprecated on 2022-08-05 (`targets` version 0.13.1).
#'   Please use [tar_callr_args_default()] instead.
#' @details Not a user-side function. Do not invoke directly.
#'   Exported for internal purposes only.
#' @return A list of arguments to `callr_function`.
#' @param callr_function A function from the `callr` package
#'   that starts an external R process.
#' @param reporter Character of length 1, choice of reporter
#'   for [tar_make()] or a related function.
#' @examples
#' tar_callr_args_default(callr::r)
callr_args_default <- function(callr_function, reporter = NULL) {
  msg <- paste(
    "callr_args_default() is deprecated in `targets`.",
    "please use tar_callr_args_default() instead"
  )
  cli::cli_alert_danger(msg)
  tar_callr_args_default(
    callr_function = callr_function,
    reporter = reporter
  )
}
