#' @title Read a target's value from storage.
#' @export
#' @family data
#' @description Read a target's return value from its file in
#'   `_targets/objects/`. For dynamic files (i.e. `format = "file"`)
#'   the paths are returned.
#' @section Limited scope:
#'   `tar_read()` and `tar_load()`
#'   are only for exploratory analysis and literate programming,
#'   and `tar_read_raw()` and `tar_load_raw()` are only
#'   for exploratory analysis. `targets` automatically
#'   loads the correct dependencies into memory when the pipeline
#'   is running, so invoking these functions
#'   from inside a target is rarely advisable.
#' @return The target's return value from its file in
#'   `_targets/objects/`, or the paths to the custom files and directories
#'   if `format = "file"` was set.
#' @inheritParams tar_read_raw
#' @param name Symbol, name of the target to read.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
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
