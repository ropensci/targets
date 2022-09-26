#' @title Delete one or more metadata records (e.g. to rerun a target).
#' @export
#' @family clean
#' @description Delete the metadata of records in `_targets/meta/meta`
#'   but keep the return values of targets in `_targets/objects/`.
#' @details This function forces one or more targets to rerun
#'   on the next [tar_make()], regardless of the cues and regardless
#'   of how those targets are stored. After `tar_invalidate()`,
#'   you will still be able to locate the data files with [tar_path()]
#'   and manually salvage them in an emergency.
#'   However, [tar_load()] and [tar_read()] will not be able to
#'   read the data into R, and subsequent calls to [tar_make()]
#'   will attempt to rerun those targets.
#'   For patterns recorded in the metadata, all the branches
#'   will be invalidated. For patterns no longer in the metadata,
#'   branches are left alone.
#' @return `NULL` (invisibly).
#' @inheritParams tar_validate
#' @param names Names of the targets to remove from the metadata list.
#'   You can supply symbols
#'   or `tidyselect` helpers like [any_of()] and [starts_with()].
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
#' tar_invalidate(starts_with("y")) # Only invalidates y1 and y2.
#' tar_make() # y1 and y2 rerun but return same values, so z is up to date.
#' })
#' }
tar_invalidate <- function(names, store = targets::tar_config_get("store")) {
  meta <- meta_init(path_store = store)
  data <- meta$database$read_condensed_data()
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, data$name)
  tar_assert_chr(names)
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  data <- as_data_frame(data)[!(data$name %in% c(names, children)), ]
  meta$database$overwrite_storage(data)
  invisible()
}
