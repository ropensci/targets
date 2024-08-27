#' @title Target resources for custom storage formats
#' @export
#' @family resources
#' @description Create the `repository_cas` argument of `tar_resources()`
#'   to specify optional target settings for custom storage formats.
#' @details `tar_resources_repository_cas()` accepts
#'   target-specific settings to customize [tar_repository_cas()] storage
#'   repositories.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_repository_cas"`, to be supplied
#'   to the `repository_cas` argument of `tar_resources()`.
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
#'   repository = tar_repository_cas(
#'     upload = function(key, path) {
#'       if (dir.exists(path)) {
#'         stop("This CAS repository does not support directory outputs.")
#'       }
#'       if (!file.exists("cas")) {
#'         dir.create("cas", recursive = TRUE)
#'       }
#'       file.copy(path, file.path("cas", key))
#'     },
#'     download = function(key, path) {
#'       file.copy(file.path("cas", key), path)
#'     },
#'     exists = function(key) {
#'       file.exists(file.path("cas", key))
#'     }
#'   ),
#'   resources = tar_resources(
#'     repository_cas = tar_resources_repository_cas(
#'       envvars = c(AUTHENTICATION_CREDENTIALS = "...")
#'     )
#'   )
#' )
tar_resources_repository_cas <- function(
  envvars = targets::tar_option_get("resources")$repository_cas$envvars
) {
  out <- resources_repository_cas_init(
    envvars = envvars
  )
  resources_validate(out)
  out
}
