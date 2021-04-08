#' @title List saved targets
#' @export
#' @family data
#' @description List targets currently saved to `_targets/objects/`.
#'   Does not include dynamic files or cloud storage.
#' @return Character vector of targets saved to `_targets/objects/`.
#' @param names Optional `tidyselect` selector to return
#'   a tactical subset of target names.
#'   If `NULL`, all names are selected.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set(workspace = "x")
#'   list(tar_target(x, "value"))
#' }, ask = FALSE)
#' tar_make()
#' tar_objects()
#' tar_objects(starts_with("x"))
#' })
#' }
tar_objects <- function(names = NULL) {
  choices <- if_any(
    dir.exists(path_objects_dir()),
    list.files(path_objects_dir(), all.files = TRUE, no.. = TRUE),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  sort(as.character(eval_tidyselect(names_quosure, choices) %|||% choices))
}
