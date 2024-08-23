#' @title Target resources for network file systems.
#' @export
#' @family resources
#' @description In high-performance computing on network file systems,
#'   if `storage = "worker"` in [tar_target()] or [tar_option_set()], then
#'   `targets` waits for hashes to synchronize before continuing the pipeline.
#'   These resources control the retry mechanism.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_network"`, to be supplied
#'   to the network argument of `tar_resources()`.
#' @param seconds_interval Positive numeric of length 1, seconds between
#'   retries.
#' @param seconds_timeout Positive numeric of length 1. Timeout length in
#'   seconds.
#' @param max_tries Positive integer of length 1. Max number of tries.
#' @param verbose Logical of length 1, whether to print informative
#'   console messages.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name = your_name,
#'   command = your_command(),
#'   storage = "worker",
#'   resources = tar_resources(
#'     network = tar_resources_network(max_tries = 3)
#'   )
#' )
#' }
tar_resources_network <- function(
  max_tries = targets::tar_option_get("resources")$network$max_tries,
  seconds_interval = targets::tar_option_get(
    "resources"
  )$network$seconds_interval,
  seconds_timeout = targets::tar_option_get(
    "resources"
  )$network$seconds_timeout,
  verbose = targets::tar_option_get("resources")$network$verbose
) {
  out <- resources_network_init(
    seconds_interval = seconds_interval %|||% 0.25,
    seconds_timeout = seconds_timeout %|||% 60,
    max_tries = max_tries %|||% 60L,
    verbose = verbose %|||% TRUE
  )
  resources_validate(out)
  out
}
