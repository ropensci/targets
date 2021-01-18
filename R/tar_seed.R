#' @title Get the random number generator seed of the target currently running.
#' @export
#' @description Get the random number generator seed
#'   of the target currently running.
#' @return Integer of length 1. If invoked inside a `targets` pipeline,
#'   the return value is the seed of the target currently running,
#'   which is a deterministic function of the target name. Otherwise,
#'   the return value is `default`.
#' @param default Integer, value to return if `tar_seed()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_seed()
#' tar_seed(default = 123L)
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(tar_target(returns_seed, tar_seed()))
#' tar_make()
#' tar_read(returns_seed)
#' })
#' }
tar_seed <- function(default = 1L) {
  default <- as.integer(default)
  assert_int(default)
  assert_scalar(default)
  trn(
    exists(x = "target", envir = tar_envir_run, inherits = FALSE),
    get(x = "target", envir = tar_envir_run)$command$seed,
    as.integer(default)
  )
}
