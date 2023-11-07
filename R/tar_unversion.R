#' @title Delete cloud object version IDs from local metadata.
#' @export
#' @family clean
#' @description Delete version IDs from local metadata.
#' @section Deleting cloud target data:
#'   Some buckets in Amazon S3 or Google Cloud Storage are "versioned",
#'   which means they track historical versions of each data object.
#'   If you use `targets` with cloud storage
#'   (<https://books.ropensci.org/targets/cloud-storage.html>)
#'   and versioning is turned on, then `targets` will record each
#'   version of each target in its metadata.
#'
#'   By default, functions [tar_delete()] and [tar_destroy()] only remove
#'   the current version ID of each target as recorded in the local
#'   metadata. Extra steps are required to remove the *latest* version
#'   of each object, whatever that version may be:
#'
#'   1. Make sure your local copy of the metadata is current and
#'     up to date. You may need to run [tar_meta_download()] or
#'     [tar_meta_sync()].
#'   2. Run [tar_unversion()] to remove the recorded version IDs of
#'     your targets in the local metadata.
#'   3. With the version IDs gone, [tar_delete()] and [tar_destroy()]
#'     will delete all the versions of the affected targets.
#' @return `NULL` (invisibly).
#' @param names Tidyselect expression to identify the targets to drop
#'   version IDs.
#' @inheritParams tar_validate
tar_unversion <- function(
  names = tidyselect::everything(),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_unversion", store)
  tar_assert_store(store = store)
  tar_assert_path(path_meta(store))
  meta <- meta_init(path_store = store)
  data <- as.data.frame(meta$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, data$name)
  tar_assert_chr(names, "names arg of tar_unversion() must eval to chr")
  replacement <- "version="
  pattern <- paste0("^", replacement, ".*")
  unversion <- data$name %in% names &
    !is.na(data$repository) &
    data$repository != "local"
  for (index in which(unversion)) {
    data$path[[index]] <- gsub(
      pattern = pattern,
      replacement = replacement,
      x = data$path[[index]]
    )
  }
  meta$database$overwrite_storage(data)
  invisible()
}
