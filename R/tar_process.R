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
#' @inheritSection tar_meta Storage access
#' @return A data frame with metadata on the most recent main R process
#'   to orchestrate the targets of the current project.
#'   The output includes the `pid` of the main process.
#' @inheritParams tar_validate
#' @param names Optional, names of the data points to return.
#'   If supplied, `tar_process()`
#'   returns only the rows of the names you select.
#'   The object supplied to `names` should be `NULL` or a
#'   `tidyselect` expression like [any_of()] or [starts_with()].
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
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
  tar_assert_allow_meta("tar_process", store)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  out <- tibble::as_tibble(process_init(path_store = store)$read_process())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, out$name)
  if (!is.null(names)) {
    out <- out[match(names, out$name), , drop = FALSE] # nolint
  }
  out
}
