callr_outer <- function(
  targets_function,
  targets_arguments,
  callr_function,
  callr_arguments
) {
  tryCatch(
    callr_dispatch(
      targets_function,
      targets_arguments,
      callr_function,
      callr_arguments
    ),
    callr_error = function(e) {
      throw_run(
        conditionMessage(e),
        "\nVisit https://books.ropensci.org/targets/debugging.html ",
        "for debugging advice."
      )
    }
  )
}

callr_dispatch <- function(
  targets_function,
  targets_arguments,
  callr_function,
  callr_arguments
) {
  assert_script()
  targets_options <- list(crayon.enabled = interactive())
  callr_arguments$func <- callr_inner
  callr_arguments$args <- list(
    targets_script = path_script(),
    targets_function = targets_function,
    targets_arguments = targets_arguments,
    targets_options = targets_options
  )
  if_any(
    is.null(callr_function),
    callr_inner(
      targets_script = path_script(),
      targets_function = targets_function,
      targets_arguments = targets_arguments,
      targets_options = targets_options
    ),
    do.call(
      callr_function,
      callr_prepare_arguments(callr_function, callr_arguments)
    )
  )
}

callr_inner <- function(
  targets_script,
  targets_function,
  targets_arguments,
  targets_options
) {
  tar_config <- getNamespace("targets")$tar_config
  tar_config$unset_lock()
  tar_config$ensure()
  tar_config$set_lock()
  on.exit(tar_config$unset_lock())
  withr::local_options(targets_options)
  value <- source(targets_script)$value
  targets_arguments$pipeline <- targets::as_pipeline(value)
  targets::pipeline_validate_lite(targets_arguments$pipeline)
  do.call(targets_function, targets_arguments)
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
callr_args_default <- function(callr_function, reporter = NULL) {
  if (is.null(callr_function)) {
    return(list())
  }
  out <- list(spinner = !identical(reporter, "summary"))
  out[intersect(names(out), names(formals(callr_function)))]
}
