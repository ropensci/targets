#' @title Target resources: parquet storage formats
#' @export
#' @family resources
#' @description Create the `parquet` argument of `tar_resources()``
#'   to specify optional settings for parquet data frame storage formats
#'   powered by the `arrow` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_parquet"`, to be supplied
#'   to the parquet argument of `tar_resources()`.
#' @param compression Character of length 1, `compression`
#'   argument of `arrow::write_parquet()`.
#' @param compression_level Numeric of length 1, `compression_level`
#'   argument of `arrow::write_parquet()`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "parquet",
#'   resources = tar_resources(
#'     parquet = tar_resources_parquet(compression = "lz4")
#'   )
#' )
tar_resources_parquet <- function(
  compression = "snappy",
  compression_level = NULL
) {
  out <- resources_parquet_init(
    compression = compression,
    compression_level = compression_level
  )
  resources_validate(out)
  out
}
