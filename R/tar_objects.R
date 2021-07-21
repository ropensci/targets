#' @title List saved targets
#' @export
#' @family data
#' @description List targets currently saved to `_targets/objects/`.
#'   Does not include dynamic files or cloud storage.
#' @return Character vector of targets saved to `_targets/objects/`.
#' @inheritParams tar_validate
#' @param names Optional `tidyselect` selector such as
#'   [all_of()] or [starts_with()] to return
#'   a tactical subset of target names.
#'   If `NULL`, all names are selected.
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
  store = targets::tar_config_get("store")
) {
  choices <- if_any(
    dir.exists(path_objects_dir(store)),
    list.files(path_objects_dir(store), all.files = TRUE, no.. = TRUE),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  sort(as.character(tar_tidyselect_eval(names_quosure, choices) %|||% choices))
}
