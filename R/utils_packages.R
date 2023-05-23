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
