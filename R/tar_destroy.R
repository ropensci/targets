#' @title Destroy a section or all of the `_targets/` data store in the
#'   current working directory
#' @export
#' @description Destroy `_targets/` data store in the current working
#'   directory. Optionally, just destroy part of the data store.
#' @return Nothing.
#' @param destroy Character of length 1, what to destroy. Choices:
#'   * `"all"`: destroy the entire data store.
#'   * `"meta"`: just delete the metadata file at `_targets/meta/meta`,
#'     which invalidates all the targets but keeps the data.
#'   * `"progress"`: just delete the progress data file at
#'     `_targets/meta/progress`, which resets the progress tracking info.
#'   * `"objects"`: delete all the target return values in `_targets/objects/`
#'     but keep progress and metadata. Dynamic files are not deleted this way.
#'   * `"scratch"`: temporary files saved during [tar_make()] that should
#'     automatically get deleted except if R crashed.
#'   * `"workspaces"`: compressed files in `_targets/workspaces/` with
#'     the saved workspaces of targets that errored. Only saved
#'     if `error = "workspace"` in [tar_option_set()] or [tar_target()].
#'     Load a workspace with [tar_workspace()].
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make() # Creates the _targets/ data store.
#' tar_destroy()
#' print(file.exists("_targets")) # Should be FALSE.
#' })
#' }
tar_destroy <- function(
  destroy = c("all", "meta", "progress", "objects", "scratch", "workspaces")
) {
  switch(
    match.arg(destroy),
    all = unlink(path_store(), recursive = TRUE),
    meta = unlink(path_meta()),
    progress = unlink(path_progress()),
    objects = unlink(path_objects_dir(), recursive = TRUE),
    scratch = unlink(path_scratch_dir(), recursive = TRUE),
    workspaces = unlink(path_workspaces_dir(), recursive = TRUE)
  )
  invisible()
}
