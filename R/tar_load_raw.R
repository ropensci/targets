#' @title Load the values of targets (raw version).
#' @export
#' @family data
#' @description Same as [tar_load()] except `names` is a character vector.
#'   Do not use in `knitr` or R Markdown reports with `tarchetypes::tar_knit()`
#'   or `tarchetypes::tar_render()`.
#' @return Nothing.
#' @inheritSection tar_read Limited scope
#' @inheritParams tar_read
#' @param names Character vector, names of the targets to load.
#'   Names are expected to appear in the metadata in `_targets/meta`.
#' @param branches Integer of indices of the branches to load
#'   for any targets that are patterns.
#' @param strict Logical of length 1, whether to error out
#'   if one of the selected targets cannot be loaded.
#'   Set to `FALSE` to just load the targets that can be loaded
#'   and skip the others.
#' @param silent Logical of length 1. If `silent` is `FALSE`
#'   and `strict` is `FALSE`, then a message will be printed
#'   if a target cannot be loaded, but load failures
#'   will not stop other targets from being loaded.
#' @param envir Environment to put the loaded targets.
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
#' tar_load_raw(c("y1", "y2"))
#' y1
#' y2
#' })
#' }
tar_load_raw <- function(
  names,
  branches = NULL,
  meta = tar_meta(store = store),
  strict = TRUE,
  silent = FALSE,
  envir = parent.frame(),
  store = targets::tar_config_get("store")
) {
  force(meta)
  force(envir)
  if (!length(names)) {
    cli_red_x("Identified no targets to load.")
  }
  tar_assert_chr(names)
  if (!is.null(branches)) {
    tar_assert_dbl(branches)
    tar_assert_positive(branches)
  }
  tar_assert_df(meta)
  tar_assert_scalar(strict)
  tar_assert_lgl(strict)
  tar_assert_envir(envir)
  map(
    names,
    ~tar_load_target(
      name = .x,
      branches = branches,
      meta = meta,
      strict = strict,
      silent = silent,
      envir = envir,
      path_store = store
    )
  )
  invisible()
}

tar_load_target <- function(
  name,
  branches,
  meta,
  strict,
  silent,
  envir,
  path_store
) {
  if_any(
    strict,
    tar_load_try(
      name = name,
      branches = branches,
      meta = meta,
      envir = envir,
      path_store = path_store
    ),
    try(
      tar_load_try(
        name = name,
        branches = branches,
        meta = meta,
        envir = envir,
        path_store = path_store
      ),
      silent = silent
    )
  )
}

tar_load_try <- function(
  name,
  branches,
  meta,
  envir,
  path_store
) {
  object <- tar_read_inner(
    name = name,
    branches = branches,
    meta = meta,
    path_store = path_store
  )
  assign(x = name, value = object, envir = envir)
}
