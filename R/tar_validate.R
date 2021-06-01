#' @title Validate a pipeline of targets.
#' @export
#' @family inspect
#' @description Inspect the pipeline for issues and throw an error or
#'   warning if a problem is detected.
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @param callr_function A function from `callr` to start a fresh clean R
#'   process to do the work. Set to `NULL` to run in the current session
#'   instead of an external process (but restart your R session just before
#'   you do in order to clear debris out of the global environment).
#'   `callr_function` needs to be `NULL` for interactive debugging,
#'   e.g. `tar_option_set(debug = "your_target")`.
#'   However, `callr_function` should not be `NULL` for serious
#'   reproducible work.
#' @param callr_arguments A list of arguments to `callr_function`.
#' @param script Character of length 1, path to the
#'   target script file. Defaults to `tar_config_get("script")`,
#'   which in turn defaults to `_targets.R`. When you set
#'   this argument, the value of `tar_config_get("script")`
#'   is temporarily changed for the current function call.
#'   See [tar_script()],
#'   [tar_config_get()], and [tar_config_set()] for details
#'   about the target script file and how to set it
#'   persistently for a project.
#' @param store Character of length 1, path to the
#'   `targets` data store. Defaults to `tar_config_get("store")`,
#'   which in turn defaults to `_targets/`.
#'   When you set this argument, the value of `tar_config_get("store")`
#'   is temporarily changed for the current function call.
#'   See [tar_config_get()] and [tar_config_set()] for details
#'   about how to set the data store path persistently
#'   for a project.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_validate()
#' })
#' }
tar_validate <- function(
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  out <- callr_outer(
    targets_function = tar_validate_inner,
    targets_arguments = list(),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    script = script,
    store = store
  )
  invisible(out)
}
  
tar_validate_inner <- function(pipeline) {
  pipeline_validate(pipeline)
}
