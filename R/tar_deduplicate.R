#' @title Deduplicate meta and progress databases (deprecated).
#' @export
#' @keywords internal
#' @description Deprecated in version 0.3.0 (2020-03-06).
#'   Deduplication happens automatically before and after the pipeline runs.
#' @details Removes duplicated entries in the meta and progress
#'   databases in order to lighten storage. These databases are located
#'   in the `_targets/meta/meta` and `_targets/meta/progress` files,
#'   where `_targets` is the a folder at the project root.
#'   No essential data is removed, so
#'   this is simply a form of garbage collection.
#' @return Nothing.
#' @inheritParams tar_validate
#' @param meta Logical, whether to deduplicate the meta database file
#'   at `_targets/meta/meta`.
#' @param progress Logical, whether to deduplicate the progress database file
#'   at `_targets/meta/progress`.
tar_deduplicate <- function(
  meta = TRUE,
  progress = TRUE,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_deduplicate")
  tar_warn_deprecate(
    "tar_deduplicate() is deprecated in version 0.3.0 (2020-03-06). ",
    "The tar_make*() functions do enough deduplication now automatically."
  )
  tar_assert_lgl(meta, "meta arg of tar_deduplicate() must be logical.")
  tar_assert_lgl(progress, "progress arg of tar_deduplicate() must be logical.")
  if (meta) {
    meta_init(path_store = store)$database$deduplicate_storage()
  }
  if (progress) {
    progress_init(path_store = store)$database$deduplicate_storage()
  }
  invisible()
}
