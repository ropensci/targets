#' @title Target resources: nanoparquet storage format
#' @export
#' @family resources
#' @description Create the nanoparquet argument of `tar_resources()`
#'   to specify optional settings for nanoparquet data frame storage formats
#'   powered by the `nanoparquet` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_nanoparquet"`, to be supplied
#'   to the nanoparquet argument of `tar_resources()`.
#' @param compression Character of length 1, `compression`
#'   argument of `nanoparquet::write_parquet()`. Defaults to `"snappy"`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "nanoparquet",
#'   resources = tar_resources(
#'     nanoparquet = tar_resources_nanoparquet(compression = "zstd")
#'   )
#' )
tar_resources_nanoparquet <- function(
  compression = targets::tar_option_get("resources")$nanoparquet$compression
) {
  compression <- compression %|||% "snappy"
  out <- resources_nanoparquet_init(
    compression = compression
  )
  resources_validate(out)
  out
}
