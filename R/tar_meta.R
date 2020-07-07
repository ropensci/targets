#' @title Read a project's metadata.
#' @export
#' @description Read the metadata of all recorded targets and global objects.
#' @return A data frame with one row per target/object and these columns:
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
#'   * `bytes`: total file size in bytes of all files in `path`.
#'   * `time`: maximum modification time stamp over all the files in `path`.
#'   * `format`: character, one of the admissible data storage formats.
#'     See the `format` argument in the [tar_target()] help file for details.
#'   * `iteration`: character, either `"list"` or `"vector"`
#'     to describe the iteration and aggregation mode of the target.
#'     See the `iteration` argument in the [tar_target()] help file for details.
#'   * `parent`: for branches, name of the parent pattern.
#'   * `children`: list column, names of the children of targets that
#'     have them. These include buds of stems and branches of patterns.
#'   * `seconds`: number of seconds it took to run the target.
#'   * `warnings`: character string of warning messages
#'     from the last run of the target.
#'   * `error`: character string of the error message if the target errored.
#' @param names Optional, names of the targets. If supplied, `tar_meta()`
#'   only returns metadata on these targets.
#'   You can supply symbols, a character vector,
#'   or `tidyselect` helpers like [starts_with()].
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script(
#'   tar_pipeline(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' )
#' tar_make()
#' tar_meta()
#' tar_meta(starts_with("y_"))
#' })
#' }
tar_meta <- function(names = NULL) {
  assert_store()
  assert_path(file.path("_targets/meta/meta"))
  out <- tibble::as_tibble(meta_init()$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect(names_quosure, out$name)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out
}
