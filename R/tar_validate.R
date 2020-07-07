#' @title Validate a pipeline of targets.
#' @export
#' @description Inspect the pipeline for issues and throw an error or
#'   warning if a problem is detected.
#' @return Nothing.
#' @param callr_function A function from `callr` to start a fresh clean R
#'   process to do the work. Set to `NULL` to do the work in the current R
#'   session, which is not reproducible and only recommended for
#'   debugging purposes.
#' @param callr_arguments A list of arguments to `callr_function`.
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script(tar_pipeline(tar_target(x, 1 + 1)))
#' tar_validate()
#' })
#' }
tar_validate <- function(callr_function = callr::r, callr_arguments = list()) {
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  callr_outer(
    targets_function = tar_validate_inner,
    targets_arguments = list(),
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
  invisible()
}
  
tar_validate_inner <- function(pipeline) {
  pipeline_validate(pipeline)
}
