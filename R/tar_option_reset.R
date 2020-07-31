#' @title Reset target options.
#' @description Reset target options you set with [tar_options()].
#'   These options are mostly configurable default arguments to
#'   [tar_target()] and [tar_target_raw()].
#' @export
#' @return Nothing.
#' @examples
#' tar_option("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_options(format = "fst_tbl") # new default format
#' tar_option("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset the format
#' tar_target(x, 1)$settings$format
tar_option_reset <- function() {
  remove(list = names(tar_envir_options), envir = tar_envir_options)
  invisible()
}
