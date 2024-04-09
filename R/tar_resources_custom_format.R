#' @title Target resources for custom storage formats
#' @export
#' @family resources
#' @description Create the `custom_format` argument of `tar_resources()`
#'   to specify optional target settings for custom storage formats.
#' @details `tar_resources_custom_format()` accepts
#'   target-specific settings to customize [tar_format()] storage formats.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_custom_format"`, to be supplied
#'   to the `custom_format` argument of `tar_resources()`.
#' @param envvars Named character vector of environment variables.
#'   These environment variables are temporarily set just before each call to
#'   the storage methods you define in [tar_format()]. Specific methods
#'   like `read` can retrieve values from these environment variables
#'   using `Sys.getenv()`. Set `envvars` to `NULL` to omit entirely.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name = target_name,
#'   command = data.frame(x = 1),
#'   format = tar_format(
#'     read = function(path) {
#'       readRDS(file = path)
#'     },
#'     write = function(object, path) {
#'       version <- as.integer(Sys.getenv("SERIALIZATION", unset = "2"))
#'       saveRDS(object = object, file = path, version = version)
#'     }
#'   ),
#'   resources = tar_resources(
#'     custom_format = tar_resources_custom_format(
#'       envvars = c(SERIALIZATION = "3")
#'     )
#'   )
#' )
tar_resources_custom_format <- function(
  envvars = targets::tar_option_get("resources")$custom_format$envvars
) {
  out <- resources_custom_format_init(
    envvars = envvars
  )
  resources_validate(out)
  out
}
