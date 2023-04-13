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
#'   Defaults to `NULL`.
#' @param seconds_interval Nonnegative numeric of length 1,
#'   number of seconds to wait between individual retries
#'   while attempting to connect to the URL.
#' @param seconds_timeout Nonnegative numeric of length 1,
#'   number of seconds to wait before timing out while trying to
#'   connect to the URL.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
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
  handle = targets::tar_option_get("resources")$url$handle,
  seconds_interval = 1,
  seconds_timeout = 10
) {
  out <- resources_url_init(
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  resources_validate(out)
  out
}
