cli_start <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "start", prefix, name), collapse = " ")
  cli_blue_bullet(msg)
}

cli_built <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "built", prefix, name), collapse = " ")
  cli_green_bullet(msg)
}

cli_skip <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip", prefix, name), collapse = " ")
  cli_green_check(msg)
}

cli_error <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "error", prefix, name), collapse = " ")
  cli_red_x(msg)
}

cli_cancel <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "cancel", prefix, name), collapse = " ")
  cli_yellow_bullet(msg)
}

cli_uptodate <- function(time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip pipeline"), collapse = " ")
  cli_green_check(msg)
}

cli_done <- function(time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "end pipeline"), collapse = " ")
  cli_blue_bullet(msg)
}

cli_workspace <- function(name, time_stamp = FALSE) {
  time <- if_any(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "record workspace", name), collapse = " ")
  cli_blue_bullet(msg)
}

cli_blue_bullet <- function(msg) {
  symbol <- cli::col_blue(cli::symbol$bullet)
  msg <- paste(symbol, msg)
  message(msg)
}

cli_green_bullet <- function(msg) {
  symbol <- cli::col_green(cli::symbol$bullet)
  msg <- paste(symbol, msg)
  message(msg)
}

cli_green_check <- function(msg) {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- paste(symbol, msg)
  message(msg)
}

cli_yellow_bullet <- function(msg) {
  symbol <- cli::col_yellow(cli::symbol$bullet)
  msg <- paste(symbol, msg)
  message(msg)
}

cli_red_x <- function(msg) {
  symbol <- cli::col_red(cli::symbol$cross)
  msg <- paste(symbol, msg)
  message(msg)
}

cli_errored <- function(errored) {
  warn_run(
    errored,
    " targets produced errors. ",
    "Run tar_meta(fields = error) for the messages."
  )
}

cli_warned <- function(warned) {
  warn_run(
    warned,
    " targets produced warnings. ",
    "Run tar_meta(fields = warnings) for the messages."
  )
}

cli_port <- function(host, port) {
  cli::cli_ul()
  cli::cli_li("url: {.path http://{host}:{port}}")
  cli::cli_li("host: {.path {host}}")
  cli::cli_li("port: {.path {port}}")
  cli::cli_end()
}

time_stamp <- function(time = Sys.time()) {
  format(time, "%z UTC %Y-%m-%d %H:%M %OS2")
}

time_stamp_short <- function(time = Sys.time()) {
  format(time, "%H:%M %OS2")
}
