#' @title Delete locally stored target return values.
#' @export
#' @family clean
#' @description Delete the return values of targets in `_targets/objects/`.
#'   but keep the records in `_targets/meta/meta`.
#'   Dynamic files and cloud data (e.g. `format = "file"`
#'   and `format = "aws_parquet"`) are not deleted.
#' @details For patterns recorded in the metadata, all the branches
#'   will be deleted. For patterns no longer in the metadata,
#'   branches are left alone.
#' @inheritParams tar_validate
#' @param names Names of the targets to remove from `_targets/objects/`.
#'   You can supply symbols
#'   or `tidyselect` helpers like [all_of()] and [starts_with()].
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
#' tar_delete(starts_with("y")) # Only deletes y1 and y2.
#' tar_make() # y1 and y2 rebuild but return same values, so z is up to date.
#' })
#' }
tar_delete <- function(names, store = targets::tar_config_get("store")) {
  tar_assert_path(path_meta(store))
  meta <- meta_init(path_store = store)
  data <- meta$database$read_condensed_data()
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, data$name)
  tar_assert_chr(names, "names arg of tar_delete() must end up as character")
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  names <- c(names, children)
  dynamic_files <- data$name[data$format == "file"]
  names <- setdiff(names, dynamic_files)
  files <- list.files(path_objects_dir(store), all.files = TRUE)
  discard <- intersect(names, files)
  unlink(file.path(path_objects_dir(store), discard), recursive = TRUE)
  invisible()
}
