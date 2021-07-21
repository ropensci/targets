#' @title Get main process info.
#' @export
#' @family data
#' @description Get info on the most recent main R process
#'   to orchestrate the targets of the current project.
#' @details The main process is the R process invoked
#'   by [tar_make()] or similar. If `callr_function` is not `NULL`,
#'   this is an external process, and the `pid` in the return value
#'   will not agree with `Sys.getpid()` in your current interactive session.
#'   The process may or may not be alive. You may want to
#'   check the status with `tar_pid() %in% ps::ps_pids()`
#'   before running another call to [tar_make()]
#'   for the same project.
#' @return A data frame with metadata on the most recent main R process
#'   to orchestrate the targets of the current project.
#'   The output includes the `pid` of the main process.
#' @inheritParams tar_validate
#' @param names Optional, names of the data points to return.
#'    If supplied, `tar_process()`
#'   returns only the rows of the names you select.
#'   You can supply symbols
#'   or `tidyselect` helpers like [all_of()] and [starts_with()].
#'   If `NULL`, all names are selected.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_process()
#' tar_process(pid)
#' })
#' }
tar_process <- function(
  names = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_path(path_process(path_store = store))
  out <- tibble::as_tibble(process_init(path_store = store)$read_process())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, out$name)
  if (!is.null(names)) {
    out <- out[match(names, out$name),, drop = FALSE] # nolint
  }
  out
}
