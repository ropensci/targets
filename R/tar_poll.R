#' @title Repeatedly poll progress in the R console.
#' @export
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
  interval = 0.25,
  timeout = Inf,
  fields = c("started", "built", "errored", "canceled", "since")
) {
  start <- proc.time()["elapsed"]
  if (!tar_exist_progress()) {
    cli_blue_bullet("Waiting for progress data.")
    spinner <- cli::make_spinner()
    while (!tar_exist_progress() && tar_poll_go(start, timeout)) {
      Sys.sleep(interval)
      spinner$spin()
    }
    spinner$finish()
  }
  assert_scalar(interval, "interval must have length 1.")
  assert_dbl(interval, "interval must be numeric.")
  assert_positive(interval, "interval must be positive.")
  assert_scalar(timeout, "timeout must have length 1.")
  assert_dbl(timeout, "timeout must be numeric.")
  assert_positive(timeout, "timeout must be positive.")
  fields_quosure <- rlang::enquo(fields)
  text <- tar_poll_text(fields_quosure, carriage_return = FALSE)
  if (tar_poll_go(start, timeout)) {
    message(text[1])
    message(text[2], appendLF = FALSE)
  }
  while (tar_poll_go(start, timeout)) {
    text <- tar_poll_text(fields_quosure, length = nchar(text[2]))
    message(text[2], appendLF = FALSE)
    Sys.sleep(interval)
  }
  cli_blue_bullet("tar_poll() timed out.")
}

tar_poll_go <- function(start, timeout) {
  (proc.time()["elapsed"] - start) < timeout
}

tar_poll_text <- function(
  fields_quosure,
  carriage_return = TRUE,
  length = NULL
) {
  progress <- tar_progress_summary(fields = NULL)
  fields <- eval_tidyselect(fields_quosure, colnames(progress)) %|||%
    colnames(progress)
  progress <- progress[, fields, drop = FALSE]
  cols <- colnames(progress)[-ncol(progress)]
  colnames(progress)[-ncol(progress)] <- paste(cols, "|")
  for (col in seq_len(ncol(progress) - 1L)) {
    progress[[col]] <- paste(progress[[col]], "|")
  }
  out <- utils::capture.output(print(as.data.frame(progress)))
  substr(out[2L], 0L, 1L) <- " "
  nchar_start <- nchar(out[1L])
  out[1L] <- trimws(out[1L], which = "left")
  n_trimmed <- nchar_start - nchar(out[1L])
  out[2L] <- substr(out[2L], n_trimmed + 1L, nchar(out[2]))
  if (carriage_return) {
    out[2L] <- paste0("\r", out[2L])
  }
  if (!is.null(length)) {
    diff <- max(0L, length - nchar(out[2L]))
    out[2L] <- paste(c(out[2L], rep(" ", diff)), collapse = "")
  }
  out
}
# nocov end
