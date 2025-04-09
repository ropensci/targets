cli_pipeline_uptodate <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  skipped
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "skipped pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- sprintf(
      " [%s, %s skipped]",
      prettyunits::pretty_sec(seconds_elapsed),
      skipped
    )
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
      prettyunits::pretty_sec(seconds_elapsed),
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
    msg_time <- paste0(" [", prettyunits::pretty_sec(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli::cli_alert_warning(msg)
}

cli_pipeline_errored <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  completed,
  skipped
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "errored pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- sprintf(
      " [%s, %s completed, %s skipped]",
      prettyunits::pretty_sec(seconds_elapsed),
      completed,
      skipped
    )
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
    metrics <- c(metrics, prettyunits::pretty_sec(seconds_elapsed))
  }
  if (!is.null(bytes_storage) && !anyNA(bytes_storage)) {
    metrics <- c(metrics, prettyunits::pretty_bytes(bytes_storage))
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

cli_df_header <- function(x, print = TRUE) {
  msg <- cli_df_text(x)[1L]
  if_any(print, message(msg, appendLF = FALSE), msg)
}

cli_df_body <- function(x, print = TRUE) {
  msg <- cli_df_text(x)[2L]
  if_any(print, message(msg, appendLF = FALSE), msg)
}

cli_df_text <- function(x) {
  names <- names(x)
  fields <- vapply(x, as.character, FUN.VALUE = character(1L))
  nchar_names <- nchar(names)
  nchar_fields <- nchar(fields)
  diff <- nchar_fields - nchar_names
  pad_names <- strrep(" ", pmax(0L, diff))
  pad_fields <- strrep(" ", abs(pmin(0L, diff)))
  names <- paste0(names, pad_names)
  fields <- paste0(fields, pad_fields)
  line1 <- paste0(paste(names, collapse = " | "), "\n")
  line2 <- paste0("\r", paste(fields, collapse = " | "))
  c(line1, line2)
}

cli_short <- function(x, max) {
  if (nchar(x) > max) {
    x <- paste0(substr(x, 0L, max - 1L), cli::symbol$ellipsis)
  }
  x
}
