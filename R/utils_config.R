#' @title Switch config settings for a function call.
#' @export
#' @keywords internal
#' @description Exported for internal purposes only. Not a
#'   user-side function. Do not invoke directly.
#' @return `NULL` (invisibly) if a valid target script exists.
#'   Otherwise, an error of class `"tar_condition_validate"`
#'   is thrown.
#' @param script Character of length 1, path to the target script file.
#' @param store Character of length 1, path to the data store.
switch_config <- function(script = NULL, store = NULL, assert_store = TRUE) {
  if (!is.null(script)) {
    assert_script(script)
  }
  if (!is.null(store) && assert_store) {
    assert_store(store)
  }
  tar_config$unset_lock()
  old_script <- tar_config$get_script()
  old_store <- tar_config$get_store()
  tar_config$assign_script(script %|||% old_script)
  tar_config$assign_store(store %|||% old_store)
  tar_config$set_lock()
  list(script = old_script, store = old_store)
}

#' @title Restore config settings (after a function call).
#' @export
#' @keywords internal
#' @description Exported for internal purposes only. Not a
#'   user-side function. Do not invoke directly.
#' @return `NULL` (invisibly) if a valid target script exists.
#'   Otherwise, an error of class `"tar_condition_validate"`
#'   is thrown.
#' @param script Character of length 1, path to the target script file.
#' @param store Character of length 1, path to the data store.
restore_config <- function(old_config) {
  tar_config$unset_lock()
  if (!is.null(old_config$script)) {
    tar_config$assign_script(old_config$script)
  }
  if (!is.null(old_config$store)) {
    tar_config$assign_store(old_config$store)
  }
}
