#' @title Get the name of the target currently running.
#' @export
#' @family utilities
#' @description Get the name of the target currently running.
#' @return Character of length 1. If called inside a pipeline,
#'   `tar_name()` returns name of the target currently running.
#'   Otherwise, the return value is `default`.
#' @param default Character, value to return if `tar_name()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_name()
#' tar_name(default = "custom_target_name")
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target(x, tar_name()), ask = FALSE)
#' tar_make()
#' tar_read(x)
#' })
#' }
tar_name <- function(default = "target") {
  tar_assert_chr(default)
  tar_assert_scalar(default)
  if_any(
    tar_runtime$exists_target(),
    target_get_name(tar_runtime$get_target()),
    as.character(default)
  )
}
