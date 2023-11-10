#' @title Read a target's value from storage.
#' @export
#' @family data
#' @description Read a target's return value from its file in
#'   `_targets/objects/`. For dynamic files (i.e. `format = "file"`)
#'   the paths are returned.
#' @inheritSection tar_meta Storage access
#' @section Cloud target data versioning:
#'   Some buckets in Amazon S3 or Google Cloud Storage are "versioned",
#'   which means they track historical versions of each data object.
#'   If you use `targets` with cloud storage
#'   (<https://books.ropensci.org/targets/cloud-storage.html>)
#'   and versioning is turned on, then `targets` will record each
#'   version of each target in its metadata.
#'
#'   However, by default,
#'   `targets` *uses* only the latest version in the bucket.
#'   You may instead want to 
#'   use the specific version of the target recorded in the local metadata
#'   (for example, if you previously committed the metadata file
#'   `_targets/meta/meta` to version control, and now you want to roll
#'   back the code and data together to an earlier point in time).
#'   To do this, you will
#'   need to modify the `resources` argument of [tar_target()] and/or
#'   [tar_option_set()] via [tar_resources()]. In [tar_resources_aws()]
#'   or [tar_resources_gcp()], set the `version` argument to `"meta"`.
#'   Modifying your code this way in `_targets.R` will control functions that
#'   read `_targets.R` when they run, such as [tar_make()], [tar_outdated()],
#'   and [tar_visnetwork()]. To apply `version = "meta"` to functions that
#'   do not read `_targets.R`, such as [tar_read()] and [tar_load()],
#'   set `resources` in [tar_option_set()] in your local R session.
#'   You can do this manually, or if you coded those options in `_targets.R`,
#'   you can manually run `_targets.R` using [tar_load_globals()].
#' @return The target's return value from its file in
#'   `_targets/objects/`, or the paths to the custom files and directories
#'   if `format = "file"` was set.
#' @inheritParams tar_read_raw
#' @param name Symbol, name of the target to read.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make()
#' tar_read(x)
#' })
#' }
tar_read <- function(
  name,
  branches = NULL,
  meta = tar_meta(store = store),
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
