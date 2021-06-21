#' @title Repeatedly poll progress in the R console.
#' @export
#' @family progress
#' @description Print the information in [tar_progress_summary()]
#'   at regular intervals.
#' @inheritParams tar_progress_summary
#' @param interval Number of seconds to wait between iterations
#'   of polling progress.
#' @param timeout How many seconds to run before exiting.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(100)),
#'     tar_target(y, Sys.sleep(0.1), pattern = map(x))
#'   )
#' }, ask = FALSE)
#' px <- tar_make(callr_function = callr::r_bg, reporter = "silent")
#' tar_poll()
#' })
#' }
# nocov start
# Covered in tests/interactive/test-tar_poll.R.
tar_poll <- function(
  interval = 1,
  timeout = Inf,
  fields = c("skipped", "started", "built", "errored", "canceled", "since"),
  store = targets::tar_config_get("store")
) {
  start <- proc.time()["elapsed"]
  if (!tar_exist_progress(store = store)) {
    cli_blue_bullet(
      paste0(
        "Waiting for progress data in ",
        file.path(store, "meta", "progress")
      )
    )
    spinner <- cli::make_spinner()
    while (!tar_exist_progress(store = store) && tar_poll_go(start, timeout)) {
      Sys.sleep(interval)
      spinner$spin()
    }
    spinner$finish()
  }
  tar_assert_scalar(interval, "interval must have length 1.")
  tar_assert_dbl(interval, "interval must be numeric.")
  tar_assert_positive(interval, "interval must be positive.")
  tar_assert_scalar(timeout, "timeout must have length 1.")
  tar_assert_dbl(timeout, "timeout must be numeric.")
  tar_assert_positive(timeout, "timeout must be positive.")
  fields_quosure <- rlang::enquo(fields)
  if (tar_poll_go(start, timeout)) {
    tar_poll_header(fields_quosure, store = store)
  }
  while (tar_poll_go(start, timeout)) {
    text <- "Lost connection to progress file."
    df <- data.frame(text = text)
    colnames(df) <- make.names(text)
    if_any(
      tar_exist_progress(store = store),
      tar_poll_body(fields_quosure, store = store),
      cli_df_body(df)
    )
    Sys.sleep(interval)
  }
  message("")
  cli_blue_bullet(paste("tar_poll() timed out after", timeout, "seconds."))
}

tar_poll_go <- function(start, timeout) {
  (proc.time()["elapsed"] - start) < timeout
}

tar_poll_df <- function(fields_quosure, store) {
  progress <- tar_progress_summary(fields = NULL, store = store)
  fields <- tar_tidyselect_eval(fields_quosure, colnames(progress)) %|||%
    colnames(progress)
  progress[, fields, drop = FALSE]
}

tar_poll_header <- function(fields_quosure, store) {
  progress <- tar_poll_df(fields_quosure, store = store)
  cli_df_header(progress)
}

tar_poll_body <- function(fields_quosure, store) {
  progress <- tar_poll_df(fields_quosure, store = store)
  cli_df_body(progress)
}
# nocov end
