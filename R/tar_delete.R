#' @title Delete target output values.
#' @export
#' @family clean
#' @description Delete the output values of targets in `_targets/objects/`
#'   (or the cloud if applicable)
#'   but keep the records in `_targets/meta/meta`.
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
#' @inheritParams tar_validate
#' @param names Names of the targets to remove from `_targets/objects/`.
#'   You can supply symbols
#'   or `tidyselect` helpers like [any_of()] and [starts_with()].
#' @param cloud Logical of length 1, whether to delete objects
#'   from the cloud if applicable (e.g. AWS, GCP). If `FALSE`,
#'   files are not deleted from the cloud.
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
#' tar_make() # y1 and y2 rebuild but return same values, so z is up to date.
#' })
#' }
tar_delete <- function(
  names,
  cloud = TRUE,
  store = targets::tar_config_get("store")
) {
  tar_assert_store(store = store)
  tar_assert_path(path_meta(store))
  meta <- meta_init(path_store = store)$database$read_condensed_data()
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
  if (cloud) {
    tar_delete_cloud(names = names, meta = meta, path_store = store)
  }
  files <- list.files(path_objects_dir(store), all.files = TRUE)
  discard <- intersect(names, files)
  unlink(file.path(path_objects_dir(store), discard), recursive = TRUE)
  invisible()
}

# Tested in tests/aws/test-delete.R
# nocov start
tar_delete_cloud <- function(names, meta, path_store) {
  index_cloud <- !is.na(meta$repository) & meta$repository != "local"
  meta <- meta[index_cloud,, drop = FALSE] # nolint
  meta <- meta[meta$name %in% names,, drop = FALSE] # nolint
  map(
    meta$name,
    ~tar_delete_cloud_target(name = .x, meta = meta, path_store = store)
  )
}

tar_delete_cloud_target <- function(name, meta, path_store) {
  row <- meta[meta$name == name,, drop = FALSE] # nolint
  record <- record_from_row(row = row, path_store = path_store)
  store <- record_bootstrap_store(record)
  store_delete_object(store = store, name = name)
}
# nocov end
