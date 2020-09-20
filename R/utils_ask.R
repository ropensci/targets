tar_should_overwrite <- function(ask = NULL, file = character(0)) {
  ask <- ask %||% tar_ask_env() %||% interactive()
  assert_lgl(ask, "ask argument must be logical.")
  assert_scalar(ask, "ask argument must have length 1.")
  if (!file.exists(file) || !ask) {
    return(TRUE)
  }
  # nocov start
  out <- utils::menu(c("yes", "no"), title = paste0("Overwrite ", file, "?"))
  as.integer(out) == 1L
  # nocov end
}

tar_ask_env <- function() {
  x <- trimws(tolower(Sys.getenv("TAR_ASK")))
  if (x == "true") {
    return(TRUE)
  }
  if (x == "false") {
    return(FALSE)
  }
  NULL
}
