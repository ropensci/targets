cli_start <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "start", prefix, name), collapse = " ")
  cli_blue_bullet(msg, print = print)
}

cli_built <- function(
  name,
  prefix = NULL,
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "built", prefix, name), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, cli::col_grey(msg_time))
  }
  cli_green_bullet(msg, print = print)
}

cli_skip <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip", prefix, name), collapse = " ")
  cli_green_check(msg, print = print)
}

cli_error <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "error", prefix, name), collapse = " ")
  cli_red_x(msg, print = print)
}

cli_cancel <- function(
  name,
  prefix = NULL,
  time_stamp = FALSE,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "cancel", prefix, name), collapse = " ")
  cli_yellow_bullet(msg, print = print)
}

cli_uptodate <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, cli::col_grey(msg_time))
  }
  cli_green_check(msg, print = print)
}

cli_done <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "end pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, cli::col_grey(msg_time))
  }
  cli_blue_bullet(msg, print = print)
}

cli_empty <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "no targets found"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, cli::col_grey(msg_time))
  }
  cli_red_x(msg, print = print)
}

cli_workspace <- function(name, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "record workspace", name), collapse = " ")
  cli_blue_bullet(msg, print = print)
}

cli_blue_bullet <- function(msg, print = TRUE) {
  symbol <- cli::col_blue(cli::symbol$bullet)
  msg <- paste(symbol, cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_green_bullet <- function(msg, print = TRUE) {
  symbol <- cli::col_green(cli::symbol$bullet)
  msg <- paste(symbol, cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_green_check <- function(msg, print = TRUE) {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- paste(symbol, cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_yellow_bullet <- function(msg, print = TRUE) {
  symbol <- cli::col_yellow(cli::symbol$bullet)
  msg <- paste(symbol, cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_mark_info <- function(msg, print = TRUE) {
  symbol <- cli::col_cyan(cli::symbol$info)
  msg <- paste(symbol, cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_blank <- function(msg, print = TRUE) {
  msg <- paste(" ", cli_color_text(msg))
  if_any(print, message(msg), msg)
}

cli_red_x <- function(msg, print = TRUE) {
  symbol <- cli::col_red(cli::symbol$cross)
  msg <- paste(symbol, cli::col_red(msg))
  if_any(print, message(msg), msg)
}

cli_errored <- function(errored) {
  tar_warn_run(
    errored,
    " targets produced errors. ",
    "Run targets::tar_meta(fields = error, complete_only = TRUE) ",
    "for the messages."
  )
}

cli_warned <- function(warned) {
  tar_warn_run(
    warned,
    " targets produced warnings. ",
    "Run targets::tar_meta(fields = warnings, complete_only = TRUE) ",
    "for the messages."
  )
}

cli_port <- function(host, port) {
  cli::cli_ul()
  cli::cli_li("url: {.path http://{host}:{port}}")
  cli::cli_li("host: {.path {host}}")
  cli::cli_li("port: {.path {port}}")
  cli::cli_end()
}

cli_df_header <- function(x, print = TRUE) {
  msg <- cli_color_text(cli_df_text(x)[1L])
  if_any(print, message(msg, appendLF = FALSE), msg)
}

cli_df_body <- function(x, print = TRUE) {
  msg <- cli_color_text(cli_df_text(x)[2L])
  if_any(print, message(msg, appendLF = FALSE), msg)
}

# nocov start
# Covered in tests/interactive/test-reporter.R.
cli_df_body_oneline <- function(x, print = TRUE) {
  msg <- paste(paste(names(x), x, sep = ": "), collapse = " | ")
  msg <- cli_color_text(msg)
  msg <- cli_color_text(paste0("\r", msg))
  if_any(print, message(msg, appendLF = FALSE), msg)
}
# nocov end

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

cli_color_text <- function(msg) {
  cli::col_none(msg)
}
