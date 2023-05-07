#' @title Random TCP port
#' @export
#' @keywords internal
#' @description Not a user-side function. Exported for infrastructure
#'   purposes only.
#' @return A random port not likely to be used by another process.
#' @param lower Integer of length 1, lowest possible port.
#' @param upper Integer of length 1, highest possible port.
#' @examples
#' if (requireNamespace("parallelly", quietly = TRUE)) {
#' tar_random_port()
#' }
tar_random_port <- function(lower = 49152L, upper = 65355L) {
  tar_assert_package("parallelly")
  ports <- seq.int(from = lower, to = upper, by = 1L)
  parallelly::freePort(ports = ports, default = NA_integer_)
}
