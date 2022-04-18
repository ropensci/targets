#' @title Load the values of all available targets.
#' @export
#' @family data
#' @description Shorthand for `tar_load(everything())` to load all
#'   targets with entries in the metadata.
#' @return Nothing.
#' @inheritSection tar_read Limited scope
#' @inheritParams tar_load
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' ls() # Does not have "y1", "y2", or "z".
#' tar_load_everything()
#' ls() # Has "y1", "y2", and "z".
#' })
#' }
tar_load_everything <- function(
  branches = NULL,
  meta = tar_meta(targets_only = TRUE, store = store),
  strict = TRUE,
  silent = FALSE,
  envir = parent.frame(),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_load(
    everything(),
    branches = branches,
    meta = meta,
    strict = strict,
    silent = silent,
    envir = envir,
    store = store
  )
}