#' @title Invalidate targets and global objects in the metadata.
#' @export
#' @family clean
#' @description Delete the metadata of records in `_targets/meta/meta`
#'   but keep the return values of targets in `_targets/objects/`.
#' @details For patterns recorded in the metadata, all the branches
#'   will be invalidated. For patterns no longer in the metadata,
#'   branches are left alone.
#' @param names Names of the targets to remove from the metadata list.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
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
#' tar_make() # y1 and y2 rebuild but return same values, so z is up to date.
#' })
#' }
tar_invalidate <- function(names) {
  assert_store()
  meta <- meta_init()
  data <- meta$database$read_condensed_data()
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, data$name)
  assert_chr(names, "names arg of tar_invalidate() must end up as character")
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  data <- as_data_frame(data)[!(data$name %in% c(names, children)), ]
  meta$database$overwrite_storage(data)
  invisible()
}
