#' @title Load the values of targets.
#' @export
#' @family data
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For dynamic files (i.e.
#'   `format = "file"`) the paths loaded in place of the values.
#'   [tar_load_everything()] is shorthand for `tar_load(everything())`
#'   to load all targets.
#' @return Nothing.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_read Cloud target data versioning
#' @inheritParams tar_load_raw
#' @param names Names of the targets to load.
#'   You may supply `tidyselect` helpers like [any_of()] and [starts_with()].
#'   Names are selected from the metadata in `_targets/meta`,
#'   which may include errored targets.
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
#' ls() # Does not have "y1", "y2", or "z".
#' tar_load(starts_with("y"))
#' ls() # Has "y1" and "y2" but not "z".
#' tar_load(any_of("z"))
#' ls() # Has "y1", "y2", and "z".
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
  tar_assert_allow_meta("tar_load", store)
  tar_assert_store(store = store)
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
