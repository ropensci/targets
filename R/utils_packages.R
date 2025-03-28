# require() is faster than library() # nolint
# but we should still fail early and loudly when needed.
load_packages <- function(packages, library) {
  out <- lapply(
    packages,
    require,
    lib.loc = library,
    quietly = TRUE,
    character.only = TRUE
  )
  out <- as.logical(unlist(out))
  if (!all(out)) {
    msg <- paste(
      "could not find packages",
      paste(packages[!out], collapse = ", "),
      "in library paths",
      paste(library, collapse = ", ")
    )
    tar_throw_validate(msg)
  }
}

package_installed <- function(package) {
  installed_packages <- .subset2(tar_runtime, "installed_packages")
  if (is.null(installed_packages)) {
    installed_packages <- lookup_new()
    tar_runtime$installed_packages <- installed_packages
  }
  result <- .subset2(installed_packages, package)
  if (is.null(result)) {
    result <- rlang::is_installed(pkg = package)
    installed_packages[[package]] <- result
  }
  result
}
