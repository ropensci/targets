#' @title Cancel a target mid-execution under a custom condition.
#' @export
#' @family utilities
#' @description Cancel a target while its command is running
#'   if a condition is met.
#' @details Must be invoked by the target itself. `tar_cancel()`
#'   cannot interrupt a target from another process.
#' @param condition Logical of length 1, whether to cancel the target.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target(x, tar_cancel(1 > 0)))
#' tar_make() # Should cancel target x.
#' })
#' }
tar_cancel <- function(condition = TRUE) {
  condition <- force(condition)
  tar_assert_lgl(condition, "condition in tar_cancel() must be logical")
  if (condition) {
    tar_throw_cancel(
      "tar_throw_cancel() is only valid inside a targets pipeline."
    )
  }
}
