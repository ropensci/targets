# RStudio addins are tested interactively in
# tests/interactive/test-rstudio_addins.R. # nolint
# nocov start
rstudio_symbol_at_cursor <- function(context) {
  if (identical(context$id, "#console")) {
    return(NULL)
  }
  cursor_pos <- context$selection[[1L]]$range$start
  cursor_line <- cursor_pos[1L]
  cursor_column <- cursor_pos[2L]
  r_symbol_pattern <- "[.A-Za-z][.A-Za-z0-9_]+"
  line_symbols <- gregexpr(
    text = context$contents[cursor_line],
    pattern = r_symbol_pattern
  )
  match_starts <- line_symbols[[1L]]
  match_ends <- match_starts + attr(x = line_symbols[[1]], "match.length") - 1L
  match_index <- which(
    cursor_column >= match_starts & cursor_column <= match_ends
  )
  if_any(
    identical(length(match_index), 0L),
    cli_red_x("Could not find object name at cursor position."),
    substr(
      context$contents[cursor_line],
      start = match_starts[match_index],
      stop = match_ends[match_index]
    )
  )
}
# nocov end
