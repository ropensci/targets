#' @title Read a target's value from storage (raw version)
#' @export
#' @family data
#' @description Like [tar_read()] except `name` is a character string.
#'   Do not use in `knitr` or R Markdown reports with `tarchetypes::tar_knit()`
#'   or `tarchetypes::tar_render()`.
#' @return The target's return value from its file in
#'   `_targets/objects/`, or the paths to the custom files and directories
#'   if `format = "file"` was set.
#' @inheritSection tar_read Limited scope
#' @inheritParams tar_validate
#' @param name Character, name of the target to read.
#' @param branches Integer of indices of the branches to load
#'   if the target is a pattern.
#' @param meta Data frame of metadata from [tar_meta()].
#'   `tar_read()` with the default arguments can be inefficient for large
#'   pipelines because all the metadata is stored in a single file.
#'   However, if you call [tar_meta()] beforehand and supply it to the `meta`
#'   argument, then successive calls to `tar_read()` may run much faster.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(list(tar_target(x, 1 + 1)), ask = FALSE)
#' tar_make()
#' tar_read_raw("x")
#' })
#' }
tar_read_raw <- function(
  name,
  branches = NULL,
  meta = tar_meta(store = store),
  store = targets::tar_config_get("store")
) {
  tar_assert_store(store = store)
  force(meta)
  tar_assert_chr(name)
  tar_read_inner(name, branches, meta, path_store = store)
}

tar_read_inner <- function(name, branches, meta, path_store) {
  index <- meta$name == name
  if (!any(index)) {
    tar_throw_validate("target ", name, " not found")
  }
  row <- meta[max(which(index)),, drop = FALSE] # nolint
  record <- record_from_row(row = row, path_store = path_store)
  if_any(
    record$type %in% c("stem", "branch"),
    read_builder(record),
    read_pattern(name, record, meta, branches, path_store)
  )
}

read_builder <- function(record) {
  store_read_object(record_bootstrap_store(record))
}

read_pattern <- function(name, record, meta, branches, path_store) {
  names <- record$children
  if (!is.null(branches)) {
    names <- names[branches]
  }
  if (length(diff <- setdiff(names, meta$name))) {
    diff <- if_any(anyNA(diff), "branches out of range", diff)
    tar_throw_validate(
      "branches not in metadata: ",
      paste(diff, collapse = ", ")
    )
  }
  meta <- meta[meta$name %in% names,, drop = FALSE] # nolint
  if (nrow(meta)) {
    meta <- meta[match(names, meta$name),, drop = FALSE] # nolint
  }
  records <- map_rows(meta, ~record_from_row(.x, path_store = path_store))
  objects <- lapply(records, read_builder)
  names(objects) <- names
  value <- value_init(iteration = record$iteration)
  value_produce_aggregate(value, objects)
}
