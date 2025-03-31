cli_pipeline_uptodate <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "skipped pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli::cli_alert_success(msg)
}

cli_pipeline_done <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  completed,
  skipped
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "ended pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- sprintf(
      " [%s, %s completed, %s skipped]",
      units_seconds(seconds_elapsed),
      completed,
      skipped
    )
    msg <- paste0(msg, msg_time)
  }
  cli::cli_alert_success(msg)
}

cli_pipeline_empty <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "empty pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli::cli_alert_warning(msg)
}

cli_pipeline_errored <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "errored pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli::cli_alert_danger(msg)
}

cli_port <- function(host, port) {
  cli::cli_ul()
  cli::cli_li("url: {.path http://{host}:{port}}")
  cli::cli_li("host: {.path {host}}")
  cli::cli_li("port: {.path {port}}")
  cli::cli_end()
}

cli_resources <- function(target) {
  seconds_elapsed <- target$metrics$seconds %|||%
    target$patternview$seconds
  bytes_storage <- target$file$bytes %|||%
    target$patternview$bytes
  metrics <- NULL
  if (!is.null(seconds_elapsed) && !anyNA(seconds_elapsed)) {
    metrics <- c(metrics, units_seconds(seconds_elapsed))
  }
  if (!is.null(bytes_storage) && !anyNA(bytes_storage)) {
    metrics <- c(metrics, units_bytes(bytes_storage))
  }
  if (!is.null(metrics)) {
    metrics <- paste0("[", paste(metrics, collapse = ", "), "]")
  }
  metrics
}

cli_url <- function(url) {
  cli::style_hyperlink(text = url, url = url)
}

cli_reset <- function() {
  on.exit(message(cli::style_reset(), appendLF = FALSE))
}

cli_plus_grey <- cli::col_grey("+")
