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

tar_assert_trust_store <- function(store) {
  if (tar_trust_store_exempt()) {
    return(invisible())
  }
  message <- paste0(
    "Data store ",
    store,
    " contains custom R code from tar_format() and/or ",
    "tar_repository_cas(), and reading these targets will run that code. ",
    "Only continue if you trust where this data store came from. ",
    "Set the TAR_ASK environment variable to \"false\" to skip this check ",
    "(e.g. usethis::edit_r_environ())."
  )
  if (!interactive()) {
    tar_throw_run(
      message,
      " Cannot ask for confirmation in a non-interactive R session."
    )
  }
  # This part is intrinsically interactive and cannot be covered
  # in fully automated tests.
  # tests/interactive/test-tar_assert_trust_store.R covers it.
  # nocov start
  choice <- utils::menu(c("yes", "no"), title = paste(message, "Run the code?"))
  if (!identical(as.integer(choice), 1L)) {
    tar_throw_run("Stopped: data store ", store, " is not trusted.")
  }
  invisible()
  # nocov end
}

# R CMD check runs examples, tests, and vignettes that the package
# developer wrote and chose to run, and it is non-interactive,
# so asking for trust there would only break checks on CRAN.
# R CMD check sets _R_CHECK_PACKAGE_NAME_ while it runs
# tests, examples, and vignettes.
tar_trust_store_exempt <- function() {
  tar_active() ||
    identical(tar_ask_env(), FALSE) ||
    tar_in_check()
}

tar_in_check <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
}

tar_assert_trust_meta <- function(meta, names, store) {
  rows <- meta$name %in% names
  if ("parent" %in% colnames(meta)) {
    rows <- rows | (meta$parent %in% names)
  }
  if (tar_meta_has_custom_code(meta[rows, , drop = FALSE])) {
    tar_assert_trust_store(store)
  }
  invisible()
}

tar_meta_has_custom_code <- function(meta) {
  any(grepl(pattern = "^format_custom", x = meta$format)) ||
    any(grepl(pattern = "^repository_cas", x = meta$repository))
}
