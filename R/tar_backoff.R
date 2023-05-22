#' @title Exponential backoff
#' @export
#' @family utilities
#' @description Configure exponential backoff while polling for tasks
#'   during the pipeline.
#' @details This function is for advanced usage only. Most users
#'   should not need to modify the default exponential backoff.
#'   To configure exponential backoff for a pipeline,
#'   supply the output of `tar_backoff()` to the `backoff` argument
#'   of [tar_option_set()] in the `_targets.R` file. See the Backoff
#'   section of the help file for details.
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
