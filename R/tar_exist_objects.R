#' @title Check if local output data exists for one or more targets.
#' @export
#' @family existence
#' @description Check if output target data exists in either
#'   `_targets/objects/` or the cloud for one or more targets.
#' @details If a target has no metadata or if the `repository`
#'   argument of [tar_target()] was set to `"local"`,
#'   then the `_targets/objects/` folder is checked. Otherwise,
#'   if there is metadata and `repsitory` is not `"local"`,
#'   then `tar_exist_objects()` checks the cloud repository
#'   selected.
#' @return Logical of length `length(names)`, whether
#'   each given target has an existing file in either
#'   `_targets/objects/` or the cloud.
#' @inheritParams tar_validate
#' @param names Character vector of target names.
#' @examples
#' tar_exist_objects(c("target1", "target2"))
tar_exist_objects <- function(
  names,
  store = targets::tar_config_get("store")
) {
  tar_assert_chr(names, "names must be a character vector.")
  meta <- if_any(file.exists(store), tar_meta(store = store), data_frame())
  out <- map_lgl(
    names,
    ~tar_exist_object(name = .x, meta = meta, store = store)
  )
  unname(out)
}

tar_exist_object <- function(name, meta, store) {
  check_local <- !(name %in% meta$name) ||
    is.na(meta$repository[meta$name == name]) ||
    identical(meta$repository[meta$name == name], "local")
  if (check_local) {
    return(file.exists(path_objects(path_store = store, name = name)))
  }
  tar_exist_cloud_target(name = name, meta = meta, path_store = store)
}

# Tested in tests/aws/test-delete.R
# nocov start
tar_exist_cloud_target <- function(name, meta, path_store) {
  row <- meta[meta$name == name,, drop = FALSE] # nolint
  record <- record_from_row(row = row, path_store = path_store)
  store <- record_bootstrap_store(record)
  store_exist_object(store = store, name = name)
}
# nocov end
