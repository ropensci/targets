#' @title Load the values of targets.
#' @export
#' @family data
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For dynamic files (i.e.
#'   `format = "file"`) the paths loaded in place of the values.
#'   `tar_load_all()` is a thin wrapper around `tar_load(everything())` to load
#'   all targets quickly.
#' @return Nothing.
#' @inheritSection tar_read Limited scope
#' @inheritParams tar_load_raw
#' @param names Names of the targets to load. You can supply symbols, a
#'   character vector, or `tidyselect` helpers like [all_of()] and
#'   [starts_with()]. Names are selected from the metadata in `_targets/meta`,
#'   which may include errored targets.
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
#' tar_load(starts_with("y"))
#' tar_load(all_of("z"))
#' })
#' }
tar_load <- function(
  names,
  branches = NULL,
  meta = tar_meta(targets_only = TRUE, store = store),
  strict = TRUE,
  silent = FALSE,
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
    strict = strict,
    silent = silent,
    envir = envir,
    store = store
  )
}

#' @export
#' @rdname tar_load
tar_load_all <- function(
  branches = NULL,
  meta = tar_meta(targets_only = TRUE, store = store),
  strict = TRUE,
  silent = FALSE,
  envir = parent.frame(),
  store = targets::tar_config_get("store")
) {
  tar_load(
    everything(),
    branches = branches,
    meta = meta,
    strict = strict,
    silent = silent,
    envir = envir,
    store = store
  )
}
