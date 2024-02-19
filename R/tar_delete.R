#' @title Delete target output values.
#' @export
#' @family clean
#' @description Delete the output values of targets in `_targets/objects/`
#'   (or the cloud if applicable)
#'   but keep the records in the metadata.
#' @details If you have a small number of data-heavy targets you
#'   need to discard to conserve storage, this function can help.
#'   Local external files files (i.e. `format = "file"`
#'   and `repository = "local"`) are not deleted.
#'   For targets with `repository` not equal `"local"`, `tar_delete()` attempts
#'   to delete the file and errors out if the deletion is unsuccessful.
#'   If deletion fails, either log into the cloud platform
#'   and manually delete the file (e.g. the AWS web console
#'   in the case of `repository = "aws"`) or call
#'   [tar_invalidate()] on that target so that `targets`
#'   does not try to delete the object.
#'   For patterns recorded in the metadata, all the branches
#'   will be deleted. For patterns no longer in the metadata,
#'   branches are left alone.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_read Cloud target data versioning
#' @inheritParams tar_validate
#' @param names Optional, names of the targets to delete. If supplied, the
#'   `names` argument restricts the targets which are deleted.
#'   The value is a `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param cloud Logical of length 1, whether to delete objects
#'   from the cloud if applicable (e.g. AWS, GCP). If `FALSE`,
#'   files are not deleted from the cloud.
#' @param batch_size Positive integer between 1 and 1000,
#'   number of target objects to delete
#'   from the cloud with each HTTP API request.
#'   Currently only supported for AWS.
#'   Cannot be more than 1000.
#' @param verbose Logical of length 1, whether to print console messages
#'   to show progress when deleting each batch of targets from each
#'   cloud bucket. Batched deletion with verbosity is currently only supported
#'   for AWS.
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
#' tar_delete(starts_with("y")) # Only deletes y1 and y2.
#' tar_make() # y1 and y2 rerun but return the same values, so z is up to date.
#' })
#' }
tar_delete <- function(
  names,
  cloud = TRUE,
  batch_size = 1000L,
  verbose = TRUE,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_delete", store)
  tar_assert_store(store = store)
  tar_assert_path(path_meta(store))
  tar_assert_lgl(cloud)
  tar_assert_scalar(cloud)
  tar_assert_none_na(cloud)
  tar_assert_dbl(batch_size)
  tar_assert_scalar(batch_size)
  tar_assert_none_na(batch_size)
  tar_assert_ge(batch_size, 1L)
  tar_assert_le(batch_size, 1000L)
  tar_assert_lgl(verbose)
  tar_assert_scalar(verbose)
  tar_assert_none_na(verbose)
  tar_message_meta(store = store)
  meta <- meta_init(path_store = store)$database$read_condensed_data()
  meta <- as.data.frame(meta)
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, meta$name)
  tar_assert_chr(names, "names arg of tar_delete() must end up as character")
  children <- unlist(meta$children[meta$name %in% names])
  children <- unique(children[!is.na(children)])
  names <- c(names, children)
  index_local_dynamic_files <- meta$format == "file" &
    meta$repository == "local"
  local_dynamic_files <- meta$name[index_local_dynamic_files]
  names <- setdiff(names, local_dynamic_files)
  names <- setdiff(names, meta$name[meta$type == "pattern"])
  if (cloud) {
    tar_delete_cloud_objects(
      names = names,
      meta = meta,
      path_store = store,
      batch_size = batch_size,
      verbose = verbose
    )
  }
  files <- list.files(path_objects_dir(store), all.files = TRUE)
  discard <- intersect(names, files)
  unlink(file.path(path_objects_dir(store), discard), recursive = TRUE)
  invisible()
}

# Tested in tests/aws/test-delete.R
# nocov start
tar_delete_cloud_objects <- function(
  names,
  meta,
  path_store,
  batch_size,
  verbose
) {
  index_cloud <- !is.na(meta$repository) & (meta$repository != "local")
  meta <- meta[index_cloud,, drop = FALSE] # nolint
  meta <- meta[meta$name %in% names,, drop = FALSE] # nolint
  meta <- meta[meta$type != "pattern",, drop = FALSE] # nolint
  for (repository in unique(meta$repository)) {
    subset <- meta[meta$repository == repository,, drop = FALSE] # nolint
    row <- subset[1L,, drop = FALSE] # nolint
    record <- record_from_row(row = row, path_store = path_store)
    store <- record_bootstrap_store(record)
    store_delete_objects(
      store = store,
      meta = subset,
      batch_size = batch_size,
      verbose = verbose
    )
  }
}
# nocov end
