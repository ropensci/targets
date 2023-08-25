#' @title Target resources for network connections
#' @export
#' @family resources
#' @description Deprecated on 2023-08-25 (`targets` 1.2.2.9000).
#'   Use [tar_resources_url()], [tar_resources_aws()], and
#'   [tar_resources_gcp()].
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_network"`, to be supplied
#'   to the network argument of `tar_resources()`.
#' @param seconds_interval Deprecated in [tar_resources_network()].
#' @param seconds_timeout Deprecated in [tar_resources_network()].
#' @param max_tries Deprecated in [tar_resources_network()].
#' @param verbose Deprecated in [tar_resources_network()].
tar_resources_network <- function(
  seconds_interval = 1,
  seconds_timeout = 60,
  max_tries = 5L,
  verbose = TRUE
) {
  tar_warn_deprecate(
    "tar_resources_network() was deprecated on 2023-08-25 ",
    "({targets} 1.2.2.9000). Please use tar_resources_url(), ",
    "tar_resources_aws(), and tar_resources_gcp()."
  )
  out <- resources_network_init(
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
  resources_validate(out)
  out
}
