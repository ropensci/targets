#' @title Superseded: exponential backoff
#' @export
#' @family utilities
#' @description Superseded: configure exponential backoff
#'   while polling for tasks during the pipeline.
#' @details This function is superseded and is now only relevant to other
#'   superseded functions [tar_make_clustermq()] and [tar_make_future()].
#'   [tar_make()] uses `crew` in an efficient non-polling way, making
#'   exponential backoff unnecessary.
#' @section Backoff:
#'   In high-performance computing it can be expensive to repeatedly poll the
#'   priority queue if no targets are ready to process. The number of seconds
#'   between polls is `runif(1, min, max(max, min * rate ^ index))`,
#'   where `index` is the number of consecutive polls so far that found
#'   no targets ready to skip or run, and `min`, `max`, and `rate`
#'   are arguments to [tar_backoff()].
#'   (If no target is ready, `index` goes up by 1. If a target is ready,
#'   `index` resets to 0. For more information on exponential,
#'   backoff, visit <https://en.wikipedia.org/wiki/Exponential_backoff>).
#'   Raising `min` or `max` is kinder to the CPU etc. but may incur delays
#'   in some instances.
#' @param min Positive numeric of length 1,
#'   minimum polling interval in seconds.
#'   Must be at least `sqrt(.Machine$double.eps)`.
#' @param max Positive numeric of length 1,
#'   maximum polling interval in seconds.
#'   Must be at least `sqrt(.Machine$double.eps)`.
#' @param rate Positive numeric of length 1, greater than or equal to 1.
#'   Multiplicative rate parameter that allows the exponential backoff
#'   minimum polling interval to increase from `min` to `max`.
#'   Actual polling intervals are sampled uniformly from the current
#'   minimum to `max`.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_option_set(backoff = tar_backoff(min = 0.001, max = 0.1, rate = 1.5))
#' })
#' }
tar_backoff <- function(min = 0.001, max = 0.1, rate = 1.5) {
  out <- backoff_init(min = min, max = max, rate = rate)
  out$validate()
  out
}
