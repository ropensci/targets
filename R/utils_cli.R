cli_target <- function(name, prefix = character(0)) {
  symbol <- cli::col_blue(cli::symbol$bullet)
  msg <- paste(symbol, prefix, name)
  message(msg)
}

cli_skip <- function(name, prefix) {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- paste(symbol, prefix, name)
  message(msg)
}

cli_error <- function(name, prefix = NULL) {
  symbol <- cli::col_red(cli::symbol$cross)
  msg <- paste(c(symbol, prefix, "error", name), collapse = " ")
  message(msg)
}

cli_cancel <- function(name, prefix = NULL) {
  symbol <- cli::col_yellow(cli::symbol$bullet)
  msg <- paste(c(symbol, prefix, "cancel", name), collapse = " ")
  message(msg)
}

cli_uptodate <- function() {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- "Already up to date."
  message(paste(symbol, msg))
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
    "run   ",
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
  running,
  skipped,
  built,
  cancelled,
  errored,
  warned
) {
  msg <- c(
    cli_tally(queued),
    cli_tally(running),
    cli_tally(skipped),
    cli_tally(built),
    cli_tally(cancelled),
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
