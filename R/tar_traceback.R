#' @title Get the traceback of the last errored target.
#' @export
#' @description Get the traceback of the last errored target.
#'   Useful for debugging purposes. Requires
#'   `callr_function = NULL` in [tar_make()].
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script(
#'    tar_pipelinetar_target(x, sqrt(sqrt(stop("error message"))))
#' )
#' # tar_make(callr_function = NULL)
#' # tar_traceback()
#' })
#' }
tar_traceback <- function() {
  if (!exists(x = "traceback", envir = envir_run, inherits = FALSE)) {
    throw_validate(
      "No traceback available. ",
      "Try running tar_make() with callr_function = NULL."
    )
  }
  get(x = "traceback", envir = envir_run)
}
