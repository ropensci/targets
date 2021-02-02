#' @title Read a target's value from storage.
#' @export
#' @description Read a target's return value from its file in
#'   `_targets/objects/`. For dynamic files (i.e. `format = "file"`)
#'   the paths are returned.
#' @return The target's return value from its file in
#'   `_targets/objects/`, or the paths to the custom files and directories
#'   if `format = "file"` was set.
#' @inheritParams tar_read_raw
#' @param name Symbol, name of the target to read.
#' @examples
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make()
#' tar_read(x)
#' })
tar_read <- function(name, branches = NULL, meta = tar_meta()) {
  name <- deparse_language(substitute(name))
  tar_read_raw(name, branches, meta)
}
