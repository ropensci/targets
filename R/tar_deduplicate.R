#' @title Deduplicate meta and progress databases (deprecated).
#' @export
#' @keywords internal
#' @description Deprecated in version 0.3.0 (2020-03-06).
#'   Deduplication happens automatically before and after the pipeline runs.
#' @details Removes duplicated entries in the meta and progress
#'   databases in order to lighten storage. These databases are located
#'   in the `_targets/meta/meta` and `_targets/meta/progress` files,
#'   and the `_targets` data store lives in the current working directory.
#'   No essential data is removed, so
#'   this is simply a form of garbage collection.
#' @return Nothing.
#' @param meta Logical, whether to deduplicate the meta database file
#'   at `_targets/meta/meta`.
#' @param progress Logical, whether to deduplicate the progress database file
#'   at `_targets/meta/progress`.
tar_deduplicate <- function(meta = TRUE, progress = TRUE) {
  warn_deprecate(
    "tar_deduplicate() is deprecated in version 0.3.0 (2020-03-06). ",
    "The tar_make*() functions do enough deduplication now automatically."
  )
  assert_store()
  assert_lgl(meta, "meta arg of tar_deduplicate() must be logical.")
  assert_lgl(progress, "progress arg of tar_deduplicate() must be logical.")
  if (meta) {
    meta_init()$database$deduplicate_storage()
  }
  if (progress) {
    progress_init()$database$deduplicate_storage()
  }
  invisible()
}
