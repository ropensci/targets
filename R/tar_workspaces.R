#' @title List saved target workspaces.
#' @export
#' @family debug
#' @description List target workspaces currently saved to
#'   `_targets/workspaces/`. See [tar_workspace()] for more information.
#' @return Character vector of available workspaces to load with
#'   [tar_workspace()].
#' @inheritParams tar_validate
#' @param names Optional `tidyselect` selector to return
#'   a tactical subset of workspace names.
#'   If `NULL`, all names are selected.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set(workspace_on_error = TRUE)
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
tar_workspaces <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  dir <- path_workspaces_dir(path_store = store)
  choices <- if_any(
    dir.exists(dir),
    sort(list.files(dir, all.files = TRUE, no.. = TRUE)),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  sort(as.character(tar_tidyselect_eval(names_quosure, choices) %|||% choices))
}
