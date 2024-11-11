#' @title Target resources: qs storage formats
#' @export
#' @family resources
#' @description Create the `qs` argument of `tar_resources()`
#'   to specify optional settings for big data storage formats
#'   powered by the `qs` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_qs"`, to be supplied
#'   to the qs argument of `tar_resources()`.
#' @param compress_level Positive integer, `compress_level` argument of
#'   [qs2::qs_save()] to control the compression level.
#' @param shuffle `TRUE` to use byte shuffling in
#'   [qs2::qs_save()] to improve compression at the cost of some
#'   computation time, `FALSE` to forgo byte shuffling.
#' @param nthreads Positive integer, number of threads to use for
#'   functions in the `qs2` package to save and read the data.
#' @param preset Deprecated in `targets` version 1.8.0.9014 (2024-11-11)
#'   and not used.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   resources = tar_resources(
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
tar_resources_qs <- function(
  compress_level = targets::tar_option_get("resources")$qs$compress_level,
  shuffle = targets::tar_option_get("resources")$qs$shuffle,
  nthreads = targets::tar_option_get("resources")$qs$nthreads,
  preset = NULL
) {
  out <- resources_qs_init(
    compress_level = compress_level %|||% 3L,
    shuffle = shuffle %|||% TRUE,
    nthreads = nthreads %|||% 1L
  )
  resources_validate(out)
  out
}
