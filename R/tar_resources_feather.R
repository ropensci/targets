#' @title Target resources: feather storage formats
#' @export
#' @family resources
#' @description Create the feather argument of `tar_resources()`
#'   to specify optional settings for feather data frame storage formats
#'   powered by the `arrow` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_feather"`, to be supplied
#'   to the feather argument of `tar_resources()`.
#' @param compression Character of length 1, `compression`
#'   argument of `arrow::write_feather()`. Defaults to `"default"`.
#' @param compression_level Numeric of length 1, `compression_level`
#'   argument of `arrow::write_feather()`.
#'   Defaults to `NULL`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "feather",
#'   resources = tar_resources(
#'     feather = tar_resources_feather(compression = "lz4")
#'   )
#' )
tar_resources_feather <- function(
  compression = targets::tar_option_get("resources")$feather$compression,
  compression_level = targets::tar_option_get(
    "resources"
  )$feather$compression_level
) {
  compression <- compression %|||% "default"
  out <- resources_feather_init(
    compression = compression,
    compression_level = compression_level
  )
  resources_validate(out)
  out
}
