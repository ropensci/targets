tar_should_delete <- function(path, ask) {
  prompt <- paste0("Delete ", path, "?")
  tar_should_modify_path(path = path, ask = ask, prompt = prompt)
}

tar_should_overwrite <- function(path, ask) {
  prompt <- paste0("Overwrite ", path, "?")
  tar_should_modify_path(path = path, ask = ask, prompt = prompt)
}

tar_should_modify_path <- function(
  path = character(0),
  ask = NULL,
  prompt = character(0)
) {
  ask <- ask %|||% tar_ask_env() %|||% interactive()
  tar_assert_lgl(ask)
  tar_assert_scalar(ask)
  prompt <- paste(
    prompt,
    "(Set the TAR_ASK environment variable to \"false\"",
    "to disable this menu, e.g. usethis::edit_r_environ().)"
  )
  if (!file.exists(path) || !ask) {
    return(TRUE)
  }
  # This part is intrinsically interactive and cannot be covered
  # in fully automated tests.
  # tests/interactive/test-tar_script.R is an example of a semi-automated test
  # that coveres this.
  # nocov start
  out <- utils::menu(c("yes", "no"), title = prompt)
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
