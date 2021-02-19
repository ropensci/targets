cli_target <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "run", prefix, name), collapse = " ")
  cli_blue_bullet(msg)
}

cli_skip <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip", prefix, name), collapse = " ")
  cli_green_check(msg)
}

cli_error <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "error", prefix, name), collapse = " ")
  cli_red_x(msg)
}

cli_cancel <- function(name, prefix = NULL, time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "cancel", prefix, name), collapse = " ")
  cli_yellow_bullet(msg)
}

cli_uptodate <- function(time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "skip pipeline"), collapse = " ")
  cli_green_check(msg)
}

cli_done <- function(time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "end pipeline"), collapse = " ")
  cli_blue_bullet(msg)
}

cli_workspace <- function(name, time_stamp = FALSE) {
  time <- trn(time_stamp, time_stamp(), NULL)
  msg <- paste(c(time, "record workspace", name), collapse = " ")
  cli_blue_bullet(msg)
}

cli_blue_bullet <- function(msg) {
  symbol <- cli::col_blue(cli::symbol$bullet)
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

cli_header_progress <- function() {
  msg <- c(
    "queue ",
    "start ",
    "skip  ",
    "built ",
    "cancel",
    "error ",
    "warn"
  )
  msg <- paste(msg, collapse = " | ")
  msg <- paste0(msg, "\n")
  message(msg, appendLF = FALSE)
}

cli_header_outdated <- function() {
  msg <- c("checked", "outdated")
  msg <- paste(msg, collapse = " | ")
  msg <- paste0(msg, "\n")
  message(msg, appendLF = FALSE)
}

cli_progress <- function(
  queued,
  started,
  skipped,
  built,
  canceled,
  errored,
  warned
) {
  msg <- c(
    cli_tally(queued),
    cli_tally(started),
    cli_tally(skipped),
    cli_tally(built),
    cli_tally(canceled),
    cli_tally(errored),
    cli_tally(warned)
  )
  msg <- paste0("\r", paste(msg, collapse = " | "))
  message(msg, appendLF = FALSE)
}

cli_outdated <- function(checked, outdated) {
  msg <- c(
    cli_tally(checked, places = 6L, vanish = -1L),
    cli_tally(outdated, places = 6L, vanish = -1L)
  )
  msg <- paste0("\r", paste(msg, collapse = " | "))
  message(msg, appendLF = FALSE)
}

cli_port <- function(host, port) {
  cli::cli_ul()
  cli::cli_li("url: {.path http://{host}:{port}}")
  cli::cli_li("host: {.path {host}}")
  cli::cli_li("port: {.path {port}}")
  cli::cli_end()
}

cli_tally <- function(x, places = 5L, vanish = 0L) {
  out <- ifelse(x > vanish, as.character(x), "")
  max <- paste(c(rep("9", places), "+"), collapse = "")
  out <- trn(nchar(out) > places, max, out)
  spaces <- paste(rep(" ", max(0L, places + 1L - nchar(out))), collapse = "")
  paste0(out, spaces)
}

time_stamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS2 %z GMT")
}
