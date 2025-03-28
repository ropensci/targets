#' @title Load the values of targets.
#' @export
#' @family storage
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For file targets (i.e.
#'   `format = "file"`) the paths loaded in place of the values.
#'   [tar_load_everything()] is shorthand for `tar_load(everything())`
#'   to load all targets.
#'
#'   [tar_load()] uses non-standard evaluation in the `names` argument
#'   (example: `tar_load(names = everything())`), whereas [tar_load_raw()]
#'   uses standard evaluation for `names`
#'   (example: `tar_load_raw(names = quote(everything()))`).
#' @return Nothing.
#' @inheritSection tar_meta Storage access
#' @inheritSection tar_read Cloud target data versioning
#' @param names Names of the targets to load.
#'   [tar_load()] uses non-standard evaluation in the `names` argument
#'   (example: `tar_load(names = everything())`), whereas [tar_load_raw()]
#'   uses standard evaluation for `names`
#'   (example: `tar_load_raw(names = quote(everything()))`).
#'
#'   The object supplied to `names` should be a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param branches Integer of indices of the branches to load
#'   for any targets that are patterns.
#' @param strict Logical of length 1, whether to error out
#'   if one of the selected targets is in the metadata
#'   but cannot be loaded.
#'   Set to `FALSE` to just load the targets in the metadata
#'   that can be loaded and skip the others.
#' @param silent Logical of length 1. Only relevant when
#'   `strict` is `FALSE`. If `silent` is `FALSE`
#'   and `strict` is `FALSE`, then a message will be printed
#'   if a target is in the metadata but cannot be loaded.
#'   However, load failures
#'   will not stop other targets from being loaded.
#' @param meta Data frame of target metadata from [tar_meta()].
#' @param envir R environment in which to load target return values.
#' @param store Character of length 1, directory path to the data store
#'   of the pipeline.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
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
#' tar_load_raw(quote(any_of("z")))
#' ls() # Has "y1", "y2", and "z".
#' })
#' }
tar_load <- function(
  names,
  branches = NULL,
  meta = targets::tar_meta(targets_only = TRUE, store = store),
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
