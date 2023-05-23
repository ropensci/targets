package_version_check <- function(package, version, repo) {
  if (!rlang::is_installed(pkg = package, version = version)) {
    package_version_message(package = package, version = version, repo = repo)
  }
}

package_version_message <- function(package, version, repo) {
  template <- paste(
    "To use {crew} with {targets},",
    "version %s or higher of packge {%s} is recommended.",
    "If the current CRAN version of {%s} is too low, you can",
    "install the development version using",
    "install.packages(\"%s\", repos = \"%s\").",
    "If you cannot install packages to the main local library, e.g. if",
    "you work in an institution with a central qualified/validated R",
    "installation, consider creating a local self-contained",
    "{renv} package library for your project. For a friendly walkthrough",
    "of {renv}, see https://rstudio.github.io/renv/articles/renv.html"
  )
  message <- sprintf(template, version, package, package, package, repo)
  cli_mark_info(message)
}

# require() is faster than library() # nolint
# but we should still fail early and loudly when needed.
load_packages <- function(packages, library) {
  out <- suppressPackageStartupMessages(
    lapply(
      packages,
      require,
      lib.loc = library,
      quietly = TRUE,
      character.only = TRUE
    )
  )
  out <- as.logical(unlist(out))
  msg <- paste(
    "could not find packages",
    paste(packages[!out], collapse = ", "),
    "in library paths",
    paste(library, collapse = ", ")
  )
  tar_assert_true(all(out), msg)
}
