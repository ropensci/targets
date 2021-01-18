#' @title Load the values of targets.
#' @export
#' @description Load the return values of targets into the current environment
#'   (or the environment of your choosing). For a typical target, the return
#'   value lives in a file in `_targets/objects/`. For dynamic files
#'   (i.e. `format = "file"`) the paths loaded in place of the values.
#' @return Nothing.
#' @inheritParams tar_load_raw
#' @param names Names of the targets to build or check. Set to `NULL` to
#'   check/build all the targets (default). Otherwise, you can supply
#'   symbols, a character vector, or `tidyselect` helpers like [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' )
#' tar_make()
#' tar_load(starts_with("y"))
#' })
#' }
tar_load <- function(
  names,
  branches = NULL,
  meta = tar_meta(),
  envir = parent.frame()
) {
  force(envir)
  names <- eval_tidyselect(rlang::enquo(names), meta$name)
  tar_load_raw(names = names, branches = branches, meta = meta, envir = envir)
}

#' @title Load the values of targets (raw version).
#' @export
#' @description Same as [tar_load()] except `names` is a character vector.
#'   Do not use in `knitr` or R Markdown reports with `tarchetypes::tar_knit()`
#'   or `tarchetypes::tar_render()`.
#' @return Nothing.
#' @inheritParams tar_read
#' @param names Character vector, names of the targets to build or check.
#' @param branches Integer of indices of the branches to load
#'   for any targets that are patterns.
#' @param envir Environment to put the loaded targets.
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tar_script(
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' )
#' tar_make()
#' tar_load_raw(c("y1", "y2"))
#' y1
#' y2
#' })
#' }
tar_load_raw <- function(
  names,
  branches = NULL,
  meta = tar_meta(),
  envir = parent.frame()
) {
  force(envir)
  if (!length(names)) {
    cli_red_x("Found no targets to load.")
  }
  assert_chr(names, "names arg of tar_load() must end up as character")
  if (!is.null(branches)) {
    assert_dbl(branches, "branches arg of tar_load() must be numeric")
    assert_positive(branches, "branches arg of tar_load() must be positive")
  }
  assert_df(meta, "meta arg of tar_load() must be a dataframe from tar_meta()")
  assert_envir(envir, "envir arg of tar_load must be an environment")
  map(names, ~tar_load_target(.x, branches, meta, envir))
  invisible()
}

tar_load_target <- function(name, branches, meta, envir) {
  object <- tar_read_inner(name = name, branches = branches, meta = meta)
  assign(x = name, value = object, envir = envir)
}
