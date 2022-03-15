#' @title List saved targets
#' @export
#' @family data
#' @description List targets currently saved to `_targets/objects/`
#'   or the cloud. Does not include local files
#'   with `tar_target(..., format = "file", repository = "local")`.
#' @return Character vector of targets saved to `_targets/objects/`.
#' @inheritParams tar_validate
#' @param names Optional `tidyselect` selector such as
#'   [all_of()] or [starts_with()] to return
#'   a tactical subset of target names.
#'   If `NULL`, all names are selected.
#' @param cloud Logical of length 1, whether to include
#'   cloud targets in the output
#'   (e.g. `tar_target(..., repository = "aws")`).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(tar_target(x, "value"))
#' }, ask = FALSE)
#' tar_make()
#' tar_objects()
#' tar_objects(starts_with("x")) # see also all_of()
#' })
#' }
tar_objects <- function(
  names = NULL,
  cloud = TRUE,
  store = targets::tar_config_get("store")
) {
  if (!file.exists(store)) {
    return(character(0))
  }
  local <- if_any(
    dir.exists(path_objects_dir(store)),
    list.files(path_objects_dir(store), all.files = TRUE, no.. = TRUE),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  local <- tar_tidyselect_eval(names_quosure, local) %|||% local
  meta <- tar_meta(store = store)
  meta <- meta[meta$repository != "local",, drop = FALSE] # nolint
  names <- tar_tidyselect_eval(names_quosure, meta$name) %|||% meta$name
  remote <- character(0)
  if (cloud) {
    exists <- map_lgl(
      names,
      ~tar_exist_cloud_target(name = .x, meta = meta, path_store = store)
    )
    remote <- names[exists]
  }
  sort(unique(as.character(c(local, remote))))
}
