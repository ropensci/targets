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
  fields = c("started", "built", "errored", "canceled", "since")
) {
  start <- proc.time()["elapsed"]
  if (!tar_exist_progress()) {
    cli_blue_bullet("Waiting for progress data in _targets/meta/progress.")
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
  if (tar_poll_go(start, timeout)) {
    tar_poll_header(fields_quosure)
  }
  while (tar_poll_go(start, timeout)) {
    tar_poll_body(fields_quosure)
    Sys.sleep(interval)
  }
  message("")
  cli_blue_bullet(paste("tar_poll() timed out after", timeout, "seconds."))
}

tar_poll_go <- function(start, timeout) {
  (proc.time()["elapsed"] - start) < timeout
}

tar_poll_df <- function(fields_quosure) {
  progress <- tar_progress_summary(fields = NULL)
  fields <- eval_tidyselect(fields_quosure, colnames(progress)) %|||%
    colnames(progress)
  progress[, fields, drop = FALSE]
}

tar_poll_header <- function(fields_quosure) {
  progress <- tar_poll_df(fields_quosure)
  cli_df_header(progress)
}

tar_poll_body <- function(fields_quosure) {
  progress <- tar_poll_df(fields_quosure)
  cli_df_body(progress)
}

cli_df_header <- function(x) {
  message(cli_df_text(x)[1L], appendLF = FALSE)
}

cli_df_body <- function(x) {
  message(cli_df_text(x)[2L], appendLF = FALSE)
}

cli_df_text <- function(x) {
  cols <- colnames(x)[-ncol(x)]
  colnames(x)[-ncol(x)] <- paste(cols, "|")
  for (col in seq_len(ncol(x) - 1L)) {
    x[[col]] <- paste(x[[col]], "|")
  }
  out <- utils::capture.output(print(as.data.frame(x)))
  substr(out[2L], 0L, 1L) <- " "
  nchar_start <- nchar(out[1L])
  out[1L] <- trimws(out[1L], which = "left")
  n_trimmed <- nchar_start - nchar(out[1L])
  out[2L] <- substr(out[2L], n_trimmed + 1L, nchar(out[2]))
  out[2L] <- paste0("\r", out[2L])
  diff <- max(0L, getOption("width") - nchar(out[2L]))
  out[1L] <- paste0(out[1L], "\n")
  out[2L] <- paste(c(out[2L], rep(" ", diff)), collapse = "")
  out
}
# nocov end
