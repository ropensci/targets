#' @title Get the random number generator seed of the target currently running.
#' @export
#' @family pseudo-random number generation
#' @description Get the random number generator seed
#'   of the target currently running.
#' @inheritSection tar_seed_create Seeds
#' @inheritSection tar_seed_create RNG overlap
#' @references
#'   Pierre L'Ecuyer, David Munger, Boris Oreshkin, and Richard Simard
#'   (2017). Random numbers for parallel computers: Requirements and methods,
#'   with emphasis on GPUs. Mathematics and Computers in Simulation,
#'   135, 3-17. \doi{10.1016/j.matcom.2016.05.005}.
#' @return Integer of length 1. If invoked inside a `targets` pipeline,
#'   the return value is the seed of the target currently running,
#'   which is a deterministic function of the target name. Otherwise,
#'   the return value is `default`.
#' @param default Integer, value to return if `tar_seed_get()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_seed_get()
#' tar_seed_get(default = 123L)
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target(returns_seed, tar_seed_get()), ask = FALSE)
#' tar_make()
#' tar_read(returns_seed)
#' })
#' }
tar_seed_get <- function(default = 1L) {
  default <- as.integer(default)
  tar_assert_int(default)
  tar_assert_scalar(default)
  if_any(
    !is.null(tar_runtime$target),
    tar_runtime$target$command$seed,
    as.integer(default)
  )
}
