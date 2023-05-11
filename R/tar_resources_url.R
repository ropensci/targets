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
#' @param seconds_interval Deprecated, nonnegative numeric of length 1,
#'   number of seconds to wait between individual retries
#'   while attempting to connect to the URL.
#'   Use [tar_resources_network()] instead.
#' @param seconds_timeout Deprecated, nonnegative numeric of length 1,
#'   number of seconds to wait before timing out while trying to
#'   connect to the URL.
#'   Use [tar_resources_network()] instead.
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
  seconds_interval = NULL,
  seconds_timeout = NULL
) {
  if (!is.null(seconds_interval) || !is.null(seconds_timeout)) {
    tar_warn_deprecate(
      "Arguments seconds_interval and seconds_timeout in tar_resources_url() ",
      "are deprecated in targets version 1.1.0 and above (2023-05-11). ",
      "Use the equivalent arguments in tar_resources_network() instead."
    )
  }
  out <- resources_url_init(
    handle = handle,
    seconds_interval = seconds_interval %|||% 1,
    seconds_timeout = seconds_timeout %|||% 10
  )
  resources_validate(out)
  out
}
