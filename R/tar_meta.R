#' @title Read a project's metadata.
#' @export
#' @family data
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
#'   If `NULL`, all names are selected.
#' @param fields Optional, names of columns/fields to select. If supplied,
#'   `tar_meta()` only returns the selected metadata columns.
#'   If `NULL`, all fields are selected.
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
#'     A target's random number generator seed
#'     is a deterministic function of its name. In this way,
#'     each target runs with a reproducible seed so someone else
#'     running the same pipeline should get the same results,
#'     and no two targets in the same pipeline share the same seed.
#'     (Even dynamic branches have different names and thus different seeds.)
#'     You can recover the seed of a completed target
#'     with `tar_meta(your_target, seed)` and run `set.seed()`
#'     on the result to locally recreate the target's initial RNG state.
#'   * `path`: A list column of paths to target data. Usually, each element
#'     is a single path, but there could be multiple paths per target
#'     for dynamic files (i.e. `tar_target(format = "file")`).
#'   * `time`: `POSIXct` object with the time the target's data in storage
#'     was last modified. Displayed in the current time zone of the system.
#'     If there are multiple outputs for that target, as with file targets,
#'     then the maximum time is shown. Only time stamps of actual
#'     targets are recorded (not functions or other global objects).
#'     No time stamp is recorded for URL targets because not all URLs
#'     have a "last-modified" time, and the ones that do have times
#'     do not necessarily adhere to a standardized format that `targets`
#'     can interpret. For the latest time stamp of a URL,
#'     use [tar_timestamp()] and specify the POSIX
#'     format and time zone that the website uses.
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
#' @param targets_only Logical, whether to just show information about targets
#'   or also return metadata on functions and other global objects.
#' @param complete_only Logical, whether to return only complete rows
#'   (no `NA` values).
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
tar_meta <- function(
  names = NULL,
  fields = NULL,
  targets_only = FALSE,
  complete_only = FALSE
) {
  assert_store()
  assert_path(path_meta())
  out <- tibble::as_tibble(meta_init()$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  fields_quosure <- rlang::enquo(fields)
  names <- eval_tidyselect(names_quosure, out$name)
  fields <- eval_tidyselect(fields_quosure, colnames(out)) %|||% colnames(out)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  if (targets_only) {
    index <- out$type %in% c("function", "object")
    out <- out[!index,, drop = FALSE] # nolint
  }
  out <- out[, base::union("name", fields), drop = FALSE]
  if (complete_only) {
    out <- out[stats::complete.cases(out),, drop = FALSE] # nolint
  }
  if ("time" %in% colnames(out)) {
    out$time <- file_time_posixct(out$time)
  }
  out
}
