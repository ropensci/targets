#' @title Reset all target options.
#' @description Reset all target options you previously chose with
#'   [tar_option_set()]. These options are mostly configurable default
#'   arguments to [tar_target()] and [tar_target_raw()].
#' @export
#' @return Nothing.
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset all options
#' tar_target(x, 1)$settings$format
tar_option_reset <- function() {
  remove(list = names(tar_envir_options), envir = tar_envir_options)
  invisible()
}
