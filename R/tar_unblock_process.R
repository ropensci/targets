#' @title Unblock the pipeline process
#' @export
#' @family utilities
#' @description `targets` tries to avoid running two concurrent instances
#'   of [tar_make()] on the same pipeline writing to the same data store.
#'   Sometimes it generates false positives (meaning [tar_make()] throws
#'   this error even though there is only one instance of the pipeline
#'   running.) If there is a false positive, [tar_unblock_process()]
#'   gets the pipeline unstuck by removing the `_targets/meta/process` file.
#'   This allows the next call to [tar_make()] to resume.
#' @return `NULL` (invisibly). Called for its side effects.
#' @param store Character string, path to the data store
#'   (usually `"_targets"`).
tar_unblock_process <- function(store = targets::tar_config_get("store")) {
  tar_assert_allow_meta("tar_unblock_process", store)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  unlink(path_process(store), force = TRUE)
}
