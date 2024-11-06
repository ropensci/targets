lookup_init <- function(true, false) {
  new.env(parent = emptyenv(), hash = TRUE)
}

lookup_true <- function(lookup, name) {
  lookup[[name]]
}

lookup_exists <- function(lookup, name) {
  !is.null(lookup[[name]])
}

lookup_set <- function(lookup, names, value) {
  for (name in names) {
    lookup[[name]] <- value
  }
}

lookup_list <- function(lookup) {
  names(lookup)
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
  tar_assert_true(is.logical(unlist(as.list(lookup))))
}
