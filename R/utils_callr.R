callr_outer <- function(
  targets_function,
  targets_arguments,
  callr_function,
  callr_arguments,
  envir,
  script
) {
  tar_assert_script(script)
  tryCatch(
    callr_dispatch(
      targets_function = targets_function,
      targets_arguments = targets_arguments,
      callr_function = callr_function,
      callr_arguments = callr_arguments,
      envir = envir,
      script = script
    ),
    callr_error = function(e) {
      tar_throw_run(
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
  callr_arguments,
  envir,
  script
) {
  options <- list(crayon.enabled = interactive())
  callr_arguments$func <- callr_inner
  callr_arguments$args <- list(
    targets_function = targets_function,
    targets_arguments = targets_arguments,
    options = options,
    script = script
  )
  if_any(
    is.null(callr_function),
    callr_inner(
      targets_function = targets_function,
      targets_arguments = targets_arguments,
      options = options,
      envir = envir,
      script = script
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
  script
) {
  force(envir)
  parent <- parent.frame()
  if (is.null(envir)) {
    envir <- parent
  }
  old_envir <- targets::tar_option_get("envir")
  targets::tar_option_set(envir = envir)
  on.exit(tar_option_set(envir = old_envir))
  withr::local_options(options)
  targets <- eval(parse(text = readLines(script)), envir = envir)
  targets_arguments$pipeline <- targets::as_pipeline(targets)
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
