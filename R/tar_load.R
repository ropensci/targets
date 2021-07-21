#' @title Load the values of targets.
#' @export
#' @family data
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For dynamic files
#'   (i.e. `format = "file"`) the paths loaded in place of the values.
#' @return Nothing.
#' @inheritSection tar_read Limited scope
#' @inheritParams tar_load_raw
#' @param names Names of the targets to load. You can supply
#'   symbols, a character vector, or `tidyselect` helpers like
#'    [all_of()] and [starts_with()].
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
#' tar_load(starts_with("y")) # see also all_of()
#' })
#' }
tar_load <- function(
  names,
  branches = NULL,
  meta = tar_meta(targets_only = TRUE, store = store),
  envir = parent.frame(),
  store = targets::tar_config_get("store")
) {
  force(meta)
  force(envir)
  names <- tar_tidyselect_eval(rlang::enquo(names), meta$name)
  tar_load_raw(
    names = names,
    branches = branches,
    meta = meta,
    envir = envir,
    store = store
  )
}
