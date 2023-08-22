#' @title Read a target's value from storage.
#' @export
#' @family data
#' @description Read a target's return value from its file in
#'   `_targets/objects/`. For dynamic files (i.e. `format = "file"`)
#'   the paths are returned.
#' @inheritSection tar_meta Storage access
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
  tar_assert_allow_meta("tar_read")
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
