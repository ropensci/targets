#' @title Reset all target options.
#' @export
#' @family configuration
#' @description Reset all target options you previously chose with
#'   [tar_option_set()]. These options are mostly configurable default
#'   arguments to [tar_target()] and [tar_target_raw()].
#' @return `NULL` (invisibly).
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset all options
#' tar_target(x, 1)$settings$format
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   tar_option_set(cue = tar_cue(mode = "always"))
#'   tar_option_reset() # Undo option above.
#'   list(tar_target(x, 1), tar_target(y, 2))
#' })
#' tar_make()
#' tar_make()
#' })
#' }
tar_option_reset <- function() {
  tar_options$reset()
  invisible()
}
