#' @title Deduplicate meta and progress databases.
#' @export
#' @description Remove duplicated entries in the meta and progress
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
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make()
#' tar_make()
#' tar_deduplicate() # Compare the file _targets/meta/meta before and after.
#' })
#' }
tar_deduplicate <- function(meta = TRUE, progress = TRUE) {
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
