#' @title Remove targets that are no longer part of the pipeline.
#' @export
#' @family clean
#' @seealso tar_prune_inspect
#' @description Remove target values from `_targets/objects/` and the cloud
#'   and remove target metadata from `_targets/meta/meta`
#'   for targets that are no longer part of the pipeline.
#' @inheritSection tar_meta Storage access
#' @details
#'   `tar_prune()` is useful if you recently worked through
#'   multiple changes to your project and are now trying to
#'   discard irrelevant data while keeping the results that still matter.
#'   Global objects and local files with `format = "file"` outside the
#'   data store are unaffected. Also removes `_targets/scratch/`,
#'   which is only needed while [tar_make()], [tar_make_clustermq()],
#'   or [tar_make_future()] is running. To list the targets that will be
#'   pruned without actually removing anything, use [tar_prune_list()].
#' @return `NULL` except if `callr_function` is `callr::r_bg`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_validate
#' @inheritParams tar_delete
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' # Remove some targets from the pipeline.
#' tar_script(list(tar_target(y1, 1 + 1)), ask = FALSE)
#' # Keep only the remaining targets in the data store.
#' tar_prune()
#' })
#' }
tar_prune <- function(
  cloud = TRUE,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_prune", store)
  force(envir)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  tar_message_meta(store = store)
  path_scratch_del(store)
  out <- callr_outer(
    targets_function = tar_prune_inner,
    targets_arguments = list(cloud = cloud, path_store = store),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_prune"
  )
  invisible(out)
}

tar_prune_inner <- function(pipeline, cloud, path_store) {
  tar_assert_store(path_store)
  names <- pipeline_get_names(pipeline)
  meta <- meta_init(path_store = path_store)
  data <- as.data.frame(meta$database$read_condensed_data())
  imports <- data$name[data$type %in% c("function", "object")]
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  keep <- c(names, children, imports)
  discard <- setdiff(data$name, keep)
  dynamic_files <- data$name[data$format == "file"]
  discard <- setdiff(discard, dynamic_files)
  discard <- setdiff(discard, data$name[data$type == "pattern"])
  if (cloud) {
    tar_delete_cloud_objects(
      names = discard,
      meta = data,
      path_store = path_store
    )
  }
  data <- as_data_frame(data)[data$name %in% keep, ]
  meta$database$overwrite_storage(data)
  unlink(file.path(path_objects_dir(path_store), discard), recursive = TRUE)
  invisible()
}
