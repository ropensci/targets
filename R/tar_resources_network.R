#' @title Target resources for network connections
#' @export
#' @family resources
#' @description Create the `network` argument of `tar_resources()`
#'   to specify optional settings for storage configurations that
#'   rely on network file systems or network connections,
#'    such as `storage = "worker"`, `format = "url"`,
#'   `repository = "aws"`, and `repository = "gcp"`.
#' @details The settings in these resources
#'   configure how to handle unreliable network connections
#'   in the case of uploading, downloading, and checking data
#'   in situations that rely on network file systems or
#'   HTTP/HTTPS requests. Specifically,
#'   they configure retries and timeouts. These settings do not
#'   apply to actions you take in the custom R command of the target.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_network"`, to be supplied
#'   to the network argument of `tar_resources()`.
#' @param seconds_interval Nonnegative numeric of length 1,
#'   number of seconds to wait between individual retries
#'   while attempting to download, upload, or check
#'   a remote network resource on a network file system (in the case of
#'   `storage = "worker"` or `format = "file"`)
#'   or via HTTP/HTTPS in cases like `format = "url"`,
#'   `repository = "aws"`, and `repository = "gcp"`.
#' @param seconds_timeout Nonnegative numeric of length 1,
#'   number of seconds to wait before timing out
#'   while attempting to download, upload, or check
#'   a remote network resource on a network file system (in the case of
#'   `storage = "worker"` or `format = "file"`)
#'   or via HTTP/HTTPS in cases like `format = "url"`,
#'   `repository = "aws"`, and `repository = "gcp"`.
#'   For files saved to a network file system in the case of
#'   `storage = "worker"`, it is recommended to set `seconds_timeout`
#'   to at least `60`.
#' @param max_tries Nonnegative numeric of length 1,
#'   maximum number of tries to download, upload, or check things
#'   while attempting to access a remote network resource on a
#'   network file system (in the case of `storage = "worker"`)
#'   or via HTTP/HTTPS in cases like `format = "url"`,
#'   `repository = "aws"`, and `repository = "gcp"`.
#' @param verbose Logical of length 1, whether to print an informative message
#'   when an HTTP/HTTPS attempt fails.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name = your_name,
#'   command = your_command(),
#'   format = "url",
#'   resources = tar_resources(
#'     network = tar_resources_network(max_tries = 3)
#'   )
#' )
#' }
tar_resources_network <- function(
  seconds_interval = 0.25,
  seconds_timeout = 60,
  max_tries = Inf,
  verbose = TRUE
) {
  out <- resources_network_init(
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
  resources_validate(out)
  out
}
