#' @title Read a project's metadata.
#' @export
#' @description Read the metadata of all recorded targets and global objects.
#' @details A metadata row only updates when the target is built.
#'   [tar_progress()] shows information on targets that are running.
#'   That is why the number of branches may disagree between [tar_meta()]
#'   and [tar_progress()] for actively running pipelines.
#' @return A data frame with one row per target/object and the selected fields.
#' @param names Optional, names of the targets. If supplied, `tar_meta()`
#'   only returns metadata on these targets.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @param fields Optional, names of columns/fields to select. If supplied,
#'   `tar_meta()` only returns the selected metadata columns.
#'   You can supply symbols, a character vector, or `tidyselect` helpers
#'   like [starts_with()]. The `name` column is always included first
#'   no matter what you select. Choices:
#'   * `name`: name of the target or global object.
#'   * `type`: type of the object: either `"function"` or `"object"`
#'     for imported global objects, and `"stem"`, `"branch"`,
#'     `"map"`, or `"cross"` for targets.
#'   * `data`: hash of the output data.
#'   * `command`: hash of the target's deparsed command.
#'   * `depend`: hash of the immediate upstream dependencies of the target.
#'   * `seed`: random number generator seed with which the target was built.
#'   * `path`: A list column of paths to target data. Usually, each element
#'     is a single path, but there could be multiple paths per target
#'     for dynamic files (i.e. `tar_target(format = "file")`).
#'   * `time`: hash of the maximum modification time stamp
#'     over all the files in `path`.
#'   * `size`: hash of the sum of all the bytes of the files at `path`.
#'   * `bytes`: total file size in bytes of all files in `path`.
#'   * `format`: character, one of the admissible data storage formats.
#'     See the `format` argument in the [tar_target()] help file for details.
#'   * `iteration`: character, either `"list"` or `"vector"`
#'     to describe the iteration and aggregation mode of the target. See the
#'     `iteration` argument in the [tar_target()] help file for details.
#'   * `parent`: for branches, name of the parent pattern.
#'   * `children`: list column, names of the children of targets that
#'     have them. These include buds of stems and branches of patterns.
#'   * `seconds`: number of seconds it took to run the target.
#'   * `warnings`: character string of warning messages
#'     from the last run of the target.
#'   * `error`: character string of the error message if the target errored.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_meta()
#' tar_meta(starts_with("y_"))
#' })
#' }
tar_meta <- function(names = NULL, fields = NULL) {
  assert_store()
  assert_path(path_meta())
  out <- tibble::as_tibble(meta_init()$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  names <- eval_tidyselect(names_quosure, out$name)
  fields <- eval_tidyselect(fields_quosure, colnames(out)) %||% colnames(out)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out[, base::union("name", fields), drop = FALSE]
}
