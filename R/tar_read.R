#' @title Read a target's value from storage.
#' @export
#' @family storage
#' @description Read a target's return value from its file in
#'   `_targets/objects/`. For file targets (i.e. `format = "file"`)
#'   the paths are returned.
#'
#'   [tar_read()] expects an
#'   unevaluated symbol for the `name` argument, whereas [tar_read_raw()]
#'   expects a character string.
#' @inheritSection tar_meta Storage access
#' @section Cloud target data versioning:
#'   Some buckets in Amazon S3 or Google Cloud Storage are "versioned",
#'   which means they track historical versions of each data object.
#'   If you use `targets` with cloud storage
#'   (<https://books.ropensci.org/targets/cloud-storage.html>)
#'   and versioning is turned on, then `targets` will record each
#'   version of each target in its metadata.
#'
#'   Functions like [tar_read()]
#'   and [tar_load()] load the version recorded in the local metadata,
#'   which may not be the same as the "current" version of the
#'   object in the bucket. Likewise, functions [tar_delete()]
#'   and [tar_destroy()] only remove
#'   the version ID of each target as recorded in the local
#'   metadata.
#'
#'   If you want to interact with the *latest* version of an object
#'   instead of the version ID recorded in the local metadata,
#'   then you will need to delete the object from the metadata.
#'
#'   1. Make sure your local copy of the metadata is current and
#'     up to date. You may need to run [tar_meta_download()] or
#'     [tar_meta_sync()] first.
#'   2. Run [tar_unversion()] to remove the recorded version IDs of
#'     your targets in the local metadata.
#'   3. With the version IDs gone from the local metadata,
#'     functions like [tar_read()] and [tar_destroy()] will use the
#'     *latest* version of each target data object.
#'   4. Optional: to back up the local metadata file with the version IDs
#'     deleted, use [tar_meta_upload()].
#' @return The target's return value from its file in
#'   `_targets/objects/`, or the paths to the custom files and directories
#'   if `format = "file"` was set.
#' @inheritParams tar_read_raw
#' @inheritParams tar_validate
#' @param name Name of the target to read.
#'   [tar_read()] expects an
#'   unevaluated symbol for the `name` argument, whereas [tar_read_raw()]
#'   expects a character string.
#' @param branches Integer of indices of the branches to load
#'   if the target is a pattern.
#' @param meta Data frame of metadata from [tar_meta()].
#'   `tar_read()` with the default arguments can be inefficient for large
#'   pipelines because all the metadata is stored in a single file.
#'   However, if you call [tar_meta()] beforehand and supply it to the `meta`
#'   argument, then successive calls to `tar_read()` may run much faster.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(tar_target(x, 1 + 1))
#' })
#' tar_make()
#' tar_read(x)
#' tar_read_raw("x")
#' })
#' }
tar_read <- function(
  name,
  branches = NULL,
  meta = targets::tar_meta(
    store = store,
    fields = -tidyselect::any_of("time")
  ),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_read", store)
  tar_assert_store(store = store)
  force(meta)
  name <- tar_deparse_language(substitute(name))
  tar_read_raw(
    name = name,
    branches = branches,
    meta = meta,
    store = store
  )
}
