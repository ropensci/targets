#' @title Destroy the data store.
#' @export
#' @family clean
#' @description Destroy the data store written by the pipeline.
#' @details The data store is a folder created by [tar_make()]
#'   (or [tar_make_future()] or [tar_make_clustermq()]).
#'   The details of the data store are explained at
#'   <https://books.ropensci.org/targets/data.html#local-data-store>.
#'   The data store folder contains the output data
#'   and metadata of the targets in the pipeline. Usually,
#'   the data store is a folder called `_targets/`
#'   (see [tar_config_set()] to customize), and it may
#'   link to data on the cloud if you used AWS or GCP
#'   buckets. By default, `tar_destroy()` deletes the entire
#'   `_targets/` folder (or wherever the data store is located),
#'   including custom user-supplied files in `_targets/user/`,
#'   as well as any cloud data that the pipeline uploaded.
#'   See the `destroy` argument to customize this behavior
#'   and only delete part of the data store, and see functions like
#'   [tar_invalidate()], [tar_delete()], and [tar_prune()] to remove
#'   information pertaining to some but not all targets in the pipeline.
#'   After calling `tar_destroy()` with default arguments,
#'   the entire data store is gone, which means all the output data from
#'   previous runs of the pipeline is gone (except for
#'   input/output files tracked with `tar_target(..., format = "file")`).
#'   The next run of the pipeline will start from scratch,
#'   and it will not skip any targets.
#' @return `NULL` (invisibly).
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
#'   * `"workspaces"`: compressed lightweight files in `workspaces/`
#'     in the data store with the saved workspaces of targets.
#'     See [tar_workspace()] for details.
#'   * `"user"`: custom user-supplied files in the `user/` folder in the
#'     data store.
#' @param ask Logical of length 1, whether to pause with a menu prompt
#'   before deleting files. To disable this menu, set the `TAR_ASK`
#'   environment variable to `"false"`. `usethis::edit_r_environ()`
#'   can help set environment variables.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
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
    "workspaces",
    "user"
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
    workspaces = path_workspaces_dir(store),
    user = path_user_dir(store)
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
