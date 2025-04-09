#' @title Show if the pipeline is running.
#' @export
#' @family utilities
#' @description Return `TRUE` if called in a target or `_targets.R` and
#'   the pipeline is running.
#' @return Logical of length 1, `TRUE` if called in a target or `_targets.R`
#'   and the pipeline is running (`FALSE` otherwise).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_active() # FALSE
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   message("Pipeline running? ", tar_active())
#'   tar_target(x, tar_active())
#' })
#' tar_manifest() # prints "Pipeline running? FALSE"
#' tar_make() # prints "pipeline running? TRUE"
#' tar_read(x) # TRUE
#' })
#' }
tar_active <- function() {
  isTRUE(tar_runtime$active)
}
