#' @title List saved target workspaces.
#' @export
#' @description List target workspaces currently saved to
#'   `_targets/workspaces/`. See [tar_workspace()] for more information.
#' @return Character vector of available workspaces to load with
#'   [tar_workspace()].
#' @param names Optional `tidyselect` selector to return
#'   a tactical subset of workspace names.
#'   If `NULL`, all names are selected.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set(error = "workspace")
#'   list(
#'     tar_target(x, "value"),
#'     tar_target(y, x)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_workspaces()
#' tar_workspaces(contains("x"))
#' })
#' }
tar_workspaces <- function(names = NULL) {
  choices <- trn(
    dir.exists(path_workspaces_dir()),
    sort(list.files(path_workspaces_dir(), all.files = TRUE, no.. = TRUE)),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  sort(as.character(eval_tidyselect(names_quosure, choices) %|||% choices))
}
