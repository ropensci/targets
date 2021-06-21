#' @title Remove targets that are no longer part of the pipeline.
#' @export
#' @family clean
#' @description Remove target values from `_targets/objects/` and
#'   target metadata from `_targets/meta/meta` for targets that are no longer
#'   part of the pipeline. Global objects and dynamic files outside the
#'   data store are unaffected. Also removes `_targets/scratch/`,
#'   which is only needed while [tar_make()], [tar_make_clustermq()],
#'   or [tar_make_future()] is running.
#' @return `NULL` except if `callr_function = callr::r_bg()`, in which case
#'   a handle to the `callr` background process is returned. Either way,
#'   the value is invisibly returned.
#' @inheritParams tar_validate
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
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
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  path_scratch_del(store)
  out <- callr_outer(
    targets_function = tar_prune_inner,
    targets_arguments = list(path_store = store),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script
  )
  invisible(out)
}

tar_prune_inner <- function(pipeline, path_store) {
  tar_assert_store(path_store)
  names <- pipeline_get_names(pipeline)
  meta <- meta_init(path_store = path_store)
  data <- meta$database$read_condensed_data()
  imports <- data$name[data$type %in% c("function", "object")]
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  keep <- c(names, children, imports)
  discard <- setdiff(data$name, keep)
  dynamic_files <- data$name[data$format == "file"]
  discard <- setdiff(discard, dynamic_files)
  data <- as_data_frame(data)[data$name %in% keep, ]
  meta$database$overwrite_storage(data)
  unlink(file.path(path_objects_dir(path_store), discard), recursive = TRUE)
  invisible()
}
