#' @title Target resources: URL storage formats
#' @export
#' @family resources
#' @description Create the `url` argument of `tar_resources()`
#'   to specify optional settings for URL storage formats.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_url"`, to be supplied
#'   to the url argument of `tar_resources()`.
#' @param handle Object returned by `curl::new_handle` or `NULL`.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "url",
#'   resources = tar_resources(
#'     url = tar_resources_url(handle = curl::new_handle())
#'   )
#' )
#' }
tar_resources_url <- function(
  handle = NULL
) {
  out <- resources_url_init(
    handle = handle
  )
  resources_validate(out)
  out
}
