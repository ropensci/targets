cli_dispatched <- function(
  name,
  prefix = NULL,
  time_stamp = FALSE,
  print = TRUE,
  pending = FALSE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  action <- if_any(pending, "dispatched (pending)", "dispatched")
  msg <- paste(c(time, action, prefix, name), collapse = " ")
  cli_blue_play(msg, print = print)
}

cli_completed <- function(
  name,
  prefix = NULL,
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  bytes_storage = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  message <- paste(c(time, "completed", prefix, name), collapse = " ")
  metrics <- character(0L)
  if (!is.null(seconds_elapsed) && !anyNA(seconds_elapsed)) {
    metrics <- c(metrics, units_seconds(seconds_elapsed))
  }
  if (!is.null(bytes_storage) && !anyNA(bytes_storage)) {
    metrics <- c(metrics, units_bytes(bytes_storage))
  }
  if (length(metrics)) {
    metrics <- paste0("[", paste(metrics, collapse = ", "), "]")
  }
  cli_green_record(paste(message, metrics), print = print)
}

cli_skip <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "skipped", prefix, name), collapse = " ")
  cli_green_check(msg, print = print)
}

cli_skip_many <- function(skipped, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  content <- sprintf("skipping targets (%s so far)...", skipped)
  msg <- paste(c(time, content), collapse = " ")
  cli_green_check(msg, print = print)
}

cli_error <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "errored", prefix, name), collapse = " ")
  cli_red_x(msg, print = print)
}

cli_cancel <- function(
  name,
  prefix = NULL,
  time_stamp = FALSE,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "canceled", prefix, name), collapse = " ")
  cli_yellow_box(msg, print = print)
}

cli_pipeline_uptodate <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "skipped pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli_green_check(msg, print = print)
}

cli_pipeline_done <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "ended pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli_blue_play(msg, print = print)
}

cli_pipeline_empty <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "empty pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli_red_x(msg, print = print)
}

cli_pipeline_errored <- function(
  time_stamp = FALSE,
  seconds_elapsed = NULL,
  print = TRUE
) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "errored pipeline"), collapse = " ")
  if (!is.null(seconds_elapsed)) {
    msg_time <- paste0(" [", units_seconds(seconds_elapsed), "]")
    msg <- paste0(msg, msg_time)
  }
  cli_red_x(msg, print = print)
}

cli_workspace <- function(name, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "recorded workspace", name), collapse = " ")
  cli_blue_play(msg, print = print)
}

cli_retry <- function(name, prefix = NULL, time_stamp = FALSE, print = TRUE) {
  time <- if_any(time_stamp, time_stamp_cli(), NULL)
  msg <- paste(c(time, "retrying", prefix, name), collapse = " ")
  cli_magenta_play(msg, print = print)
}

cli_blue_bullet <- function(msg, print = TRUE) {
  symbol <- cli_symbol_bullet_blue
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_blue_play <- function(msg, print = TRUE) {
  symbol <- cli_symbol_play_blue
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_magenta_play <- function(msg, print = TRUE) {
  symbol <- cli_symbol_play_magenta
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_green_record <- function(msg, print = TRUE) {
  symbol <- cli_symbol_record_green
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_green_check <- function(msg, print = TRUE) {
  symbol <- cli_symbol_tick_green
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_yellow_box <- function(msg, print = TRUE) {
  symbol <- cli_symbol_box_yellow
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_mark_info <- function(msg, print = TRUE) {
  symbol <- cli_symbol_info_cyan
  msg <- paste(symbol, msg)
  if_any(print, message(msg), msg)
}

cli_blank <- function(msg, print = TRUE) {
  msg <- paste(" ", msg)
  if_any(print, message(msg), msg)
}

cli_red_x <- function(msg, print = TRUE) {
  old_cli_number_ansi_colors <- getOption("cli.num_colors")
  on.exit(options(cli.num_colors = old_cli_number_ansi_colors))
  options(cli.num_colors = cli_number_ansi_colors)
  symbol <- cli_symbol_x_red
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
  msg <- cli_df_text(x)[1L]
  if_any(print, message(msg, appendLF = FALSE), msg)
}

cli_df_body <- function(x, print = TRUE) {
  msg <- cli_df_text(x)[2L]
  if_any(print, message(msg, appendLF = FALSE), msg)
}

# nocov start
# Covered in tests/interactive/test-reporter.R.
cli_forecast <- function(x, print = TRUE) {
  msg <- sprintf("\r  checked: %s | outdated: %s", x$checked, x$outdated)
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

cli_url <- function(url) {
  cli::style_hyperlink(text = url, url = url)
}

cli_symbol_bullet_blue <- cli::col_blue(cli::symbol$bullet)
cli_symbol_play_blue <- cli::col_blue(cli::symbol$play)
cli_symbol_play_magenta <- cli::col_magenta(cli::symbol$play)
cli_symbol_record_green <- cli::col_green(cli::symbol$record)
cli_symbol_box_yellow <- cli::col_yellow(cli::symbol$stop)
cli_symbol_info_cyan <- cli::col_cyan(cli::symbol$info)
cli_symbol_tick_green <- cli::col_green(cli::symbol$tick)
cli_symbol_x_red <- cli::col_red(cli::symbol$cross)
cli_number_ansi_colors <- cli::num_ansi_colors()
