#' @title Load the values of targets.
#' @export
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For dynamic files
#'   (i.e. `format = "file"`) the paths loaded in place of the values.
#' @return Nothing.
#' @inheritParams tar_load_raw
#' @param names Names of the targets to load. You can supply
#'   symbols, a character vector, or `tidyselect` helpers like [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_load(starts_with("y"))
#' })
#' }
tar_load <- function(
  names,
  branches = NULL,
  meta = tar_meta(targets_only = TRUE),
  envir = parent.frame()
) {
  force(envir)
  names <- eval_tidyselect(rlang::enquo(names), meta$name)
  tar_load_raw(names = names, branches = branches, meta = meta, envir = envir)
}
