tar_should_overwrite <- function(ask = NULL, file = character(0)) {
  ask <- ask %||% tar_ask_env() %||% interactive()
  assert_lgl(ask, "ask argument must be logical.")
  assert_scalar(ask, "ask argument must have length 1.")
  if (!file.exists(file) || !ask) {
    return(TRUE)
  }
  # This part is intrinsically interactive and cannot be covered
  # in fully automated tests.
  # tests/interactive/test-tar_script.R is an example of a semi-automated test
  # that coveres this.
  # nocov start
  out <- utils::menu(c("yes", "no"), title = paste0("Overwrite ", file, "?"))
  identical(as.integer(out), 1L)
  # nocov end
}

tar_ask_env <- function() {
  x <- trimws(tolower(Sys.getenv("TAR_ASK")))
  if (identical(x, "true")) {
    return(TRUE)
  }
  if (identical(x, "false")) {
    return(FALSE)
  }
  NULL
}
