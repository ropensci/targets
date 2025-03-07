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
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_read Cloud target data versioning
#' @return `NULL` (invisibly).
#' @inheritParams tar_delete
#' @inheritParams tar_validate
#' @param destroy Character of length 1, what to destroy. Choices:
#'   * `"all"`: entire data store (default: `_targets/`)
#'     including cloud data, as well as download/upload scratch files.
#'   * `"cloud"`: cloud data, including metadata, target object data
#'     from targets with `tar_target(..., repository = "aws")`,
#'     and workspace files saved on the cloud.
#'     Also deletes temporary staging files in
#'     `file.path(tempdir(), "targets")`
#'     that may have been accidentally left over from incomplete
#'     uploads or downloads.
#'   * `"local"`: all the local files in the data store but nothing
#'     on the cloud.
#'   * `"meta"`: metadata file at `meta/meta` in the
#'     data store, which invalidates all the targets but keeps the data.
#'   * `"process"`: progress data file at
#'     `meta/process` in the data store, which resets the metadata
#'     of the main process.
#'   * `"progress"`: progress data file at
#'     `meta/progress` in the data store,
#'     which resets the progress tracking info.
#'   * `"objects"`: all the target
#'     return values in `objects/` in the data
#'     store but keep progress and metadata.
#'     Dynamic files are not deleted this way.
#'   * `"scratch"`: temporary files in saved during [tar_make()] that should
#'     automatically get deleted except if R crashed.
#'   * `"workspaces"`: compressed lightweight files locally saved
#'     to the `workspaces/` folder
#'     in the data store with the saved workspaces of targets.
#'     Does not delete workspace files on the cloud. For that,
#'     consider `destroy = "all"` or `destroy = "cloud"`.
#'     See [tar_workspace()] for details.
#'   * `"user"`: custom user-supplied files in the `user/` folder in the
#'     data store.
#' @param ask Logical of length 1, whether to pause with a menu prompt
#'   before deleting files. To disable this menu, set the `TAR_ASK`
#'   environment variable to `"false"`. `usethis::edit_r_environ()`
#'   can help set environment variables.
#' @param script Character of length 1, path to the
#'   target script file. Defaults to `tar_config_get("script")`,
#'   which in turn defaults to `_targets.R`. If the script does not exist,
#'   then cloud metadata will not be deleted.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(tar_target(x, 1 + 1))
#' })
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
  batch_size = 1000L,
  verbose = TRUE,
  ask = NULL,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_destroy", store)
  tar_assert_dbl(batch_size)
  tar_assert_scalar(batch_size)
  tar_assert_none_na(batch_size)
  tar_assert_ge(batch_size, 1L)
  tar_assert_le(batch_size, 1000L)
  tar_assert_lgl(verbose)
  tar_assert_scalar(verbose)
  tar_assert_none_na(verbose)
  if (!file.exists(store)) {
    return(invisible())
  }
  destroy <- match.arg(destroy)
  path <- switch(
    destroy,
    all = store,
    local = store,
    cloud = path_scratch_dir_network(),
    meta = path_meta(store),
    process = path_process(store),
    progress = path_progress(store),
    objects = path_objects_dir(store),
    scratch = path_scratch_dir(store),
    workspaces = path_workspaces_dir(store),
    user = path_user_dir(store)
  )
  if (destroy %in% c("all", "cloud")) {
    meta <- suppressMessages(tar_meta(store = store))
    tar_delete_cloud_objects(
      names = meta$name,
      meta = meta,
      path_store = store,
      batch_size = batch_size,
      verbose = verbose
    )
    tar_delete_cloud_meta_and_workspaces(script = script)
    unlink(path_scratch_dir_network(), recursive = TRUE)
  }
  if (tar_should_delete(path = path, ask = ask)) {
    unlink(path, recursive = TRUE)
  }
  invisible()
}

# Covered in AWS and GCP tests.
# nocov start
tar_delete_cloud_meta_and_workspaces <- function(script) {
  if (!file.exists(script)) {
    return()
  }
  options <- tar_script_options(script = script)
  old_repository_meta <- tar_options$get_repository_meta()
  old_resources <- tar_options$get_resources()
  on.exit({
    tar_options$set_repository_meta(old_repository_meta)
    tar_options$set_resources(old_resources)
  })
  tar_option_set(repository_meta = options$repository_meta)
  tar_option_set(resources = options$resources)
  meta <- database_meta(path_store = tempfile())
  progress <- database_progress(path_store = tempfile())
  process <- database_process(path_store = tempfile())
  crew <- database_crew(path_store = tempfile())
  meta$delete_cloud(verbose = FALSE)
  progress$delete_cloud(verbose = FALSE)
  process$delete_cloud(verbose = FALSE)
  crew$delete_cloud(verbose = FALSE)
  meta$delete_cloud_workspaces()
  invisible()
}
# nocov end
