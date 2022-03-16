#' @title Destroy all or part of the data store.
#' @export
#' @family clean
#' @description Destroy all or part of the data store written
#'   by [tar_make()] and similar functions.
#' @details `tar_destroy()` is a hard reset. Use it if you
#'   intend to start the pipeline from scratch without
#'   any trace of a previous run in `_targets/`.
#'   Global objects and dynamic files outside the
#'   data store are unaffected.
#' @return Nothing.
#' @inheritParams tar_validate
#' @param destroy Character of length 1, what to destroy. Choices:
#'   * `"all"`: destroy the entire data store (default: `_targets/`)
#'     including cloud data.
#'   * `"cloud"`: just try to delete cloud data, e.g. target data
#'     from targets with `tar_target(..., repository = "aws")`.
#'   * `"local"`: all the local files in the data store but nothing
#'     on the cloud.
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
#'     the saved workspaces of targets. See [tar_workspace()] for details.
#' @param ask Logical of length 1, whether to pause with a menu prompt
#'   before deleting files. To disable this menu, set the `TAR_ASK`
#'   environment variable to `"false"`. `usethis::edit_r_environ()`
#'   can help set environment variables.
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
    "cloud",
    "local",
    "meta",
    "process",
    "progress",
    "objects",
    "scratch",
    "workspaces"
  ),
  ask = NULL,
  store = targets::tar_config_get("store")
) {
  if (!file.exists(store)) {
    return(invisible())
  }
  destroy <- match.arg(destroy)
  path <- switch(
    destroy,
    all = store,
    local = store,
    cloud = tempfile(),
    meta = path_meta(store),
    process = path_process(store),
    progress = path_progress(store),
    objects = path_objects_dir(store),
    scratch = path_scratch_dir(store),
    workspaces = path_workspaces_dir(store)
  )
  if (destroy %in% c("all", "cloud")) {
    meta <- tar_meta(store = store)
    tar_delete_cloud(names = meta$name, meta = meta, path_store = store)
  }
  if (tar_should_delete(path = path, ask = ask)) {
    unlink(path, recursive = TRUE)
  }
  invisible()
}
