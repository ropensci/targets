#' @title Deprecated: get the seed of the current target.
#' @export
#' @keywords internal
#' @description Deprecated on 2023-10-12 (`targets` version 1.3.2.9001).
#'   Use [tar_seed_get()] instead.
#' @return Integer of length 1. If invoked inside a `targets` pipeline,
#'   the return value is the seed of the target currently running,
#'   which is a deterministic function of the target name. Otherwise,
#'   the return value is `default`.
#' @inheritParams tar_seed_get
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
tar_seed <- function(default = 1L) {
  tar_warn_deprecate(
    "tar_seed() was deprecated on 2023-10-12 (targets version 1.3.2.9001). ",
    "Use tar_seed_get() instead."
  )
  tar_seed_get(default = default)
}
