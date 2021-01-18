#' @title Delete target return values.
#' @export
#' @description Delete the return values of targets in `_targets/objects/`.
#'   but keep the records in `_targets/meta/meta`.
#'   Dynamic files outside the data store are unaffected.
#'   The `_targets/` data store must be in the current working directory.
#' @details For patterns recorded in the metadata, all the branches
#'   will be deleted. For patterns no longer in the metadata,
#'   branches are left alone.
#' @param names Names of the targets to remove from `_targets/objects/`.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' )
#' tar_make()
#' tar_delete(starts_with("y")) # Only deletes y1 and y2.
#' tar_make() # y1 and y2 rebuild but return same values, so z is up to date.
#' })
#' }
tar_delete <- function(names) {
  assert_store()
  meta <- meta_init()
  data <- meta$database$read_condensed_data()
  names_quosure <- rlang::enquo(names)
  names <- eval_tidyselect(names_quosure, data$name)
  assert_chr(names, "names arg of tar_delete() must end up as character")
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  names <- c(names, children)
  dynamic_files <- data$name[data$format == "file"]
  names <- setdiff(names, dynamic_files)
  dir <- file.path("_targets", "objects")
  files <- list.files(dir, all.files = TRUE)
  discard <- intersect(names, files)
  unlink(file.path(dir, discard), recursive = TRUE)
  invisible()
}
