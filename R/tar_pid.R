#' @title Get main process ID.
#' @export
#' @family data
#' @description Get the process ID (PID) of the most recent main R process
#'   to orchestrate the targets of the current project.
#' @details The main process is the R process invoked
#'   by [tar_make()] or similar. If `callr_function` is not `NULL`,
#'   this is an external process, and the `pid` in the return value
#'   will not agree with `Sys.getpid()` in your current interactive session.
#'   The process may or may not be alive. You may want to
#'   check it with `ps::ps_is_running(ps::ps_handle(targets::tar_pid()))`
#'   before running another call to [tar_make()]
#'   for the same project.
#' @return Integer with the process ID (PID) of the most recent
#'   main R process to orchestrate the targets of the current project.
#' @inheritParams tar_validate
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
#' Sys.getpid()
#' tar_pid() # Different from the current PID.
#' })
#' }
tar_pid <- function(store = targets::tar_config_get("store")) {
  out <- tar_process(names = NULL, store = store)
  as.integer(out$value[out$name == "pid"])
}
