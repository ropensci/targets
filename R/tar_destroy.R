#' @title Destroy all or part of the data store.
#' @export
#' @family clean
#' @description Destroy all or part of the data store written
#'   by [tar_make()] and similar functions.
#' @return Nothing.
#' @inheritParams tar_validate
#' @param destroy Character of length 1, what to destroy. Choices:
#'   * `"all"`: destroy the entire data store (default: `_targets/`)
#'   * `"meta"`: just delete the metadata file at `meta/meta` in the
#'     data store, which invalidates all the targets but keeps the data.
#'   * `"process"`: just delete the progress data file at
#'     `meta/process` in the data store, which resets the metadata
#'     of the main process.
#'   * `"progress"`: just delete the progress data file at
#'     `meta/progress` in the data store,
#'     which resets the progress tracking info.
#'   * `"objects"`: delete all the target
#'     return values in `objects/` in the data
#'     store but keep progress and metadata.
#'     Dynamic files are not deleted this way.
#'   * `"scratch"`: temporary files saved during [tar_make()] that should
#'     automatically get deleted except if R crashed.
#'   * `"workspaces"`: compressed files in `workspaces/` in the data store with
#'     the saved workspaces of targets that errored. Only saved
#'     if `error = "workspace"` in [tar_option_set()] or [tar_target()].
#'     Load a workspace with [tar_workspace()].
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make() # Creates the _targets/ data store.
#' tar_destroy()
#' print(file.exists("_targets")) # Should be FALSE.
#' })
#' }
tar_destroy <- function(
  destroy = c(
    "all",
    "meta",
    "process",
    "progress",
    "objects",
    "scratch",
    "workspaces"
  ),
  store = targets::tar_config_get("store")
) {
  switch(
    match.arg(destroy),
    all = unlink(store, recursive = TRUE),
    meta = unlink(path_meta(store)),
    process = unlink(path_process(store)),
    progress = unlink(path_progress(store)),
    objects = unlink(path_objects_dir(store), recursive = TRUE),
    scratch = unlink(path_scratch_dir(store), recursive = TRUE),
    workspaces = unlink(path_workspaces_dir(store), recursive = TRUE)
  )
  invisible()
}
