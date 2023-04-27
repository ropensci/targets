set_envvars <- function(envvars) {
  map_rows(envvars, set_envvar)
}

set_envvar <- function(envvar) {
  args <- list(envvar["value"])
  names(args) <- envvar["name"]
  if (!identical(as.character(envvar["value"]), "")) {
    do.call(what = Sys.setenv, args = args)
  }
  invisible()
}

in_test <- function() {
  Sys.getenv("TESTTHAT") == "true"
}
