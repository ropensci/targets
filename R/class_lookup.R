lookup_init <- function(names) {
  list <- rep(TRUE, length(names))
  names(list) <- names
  list2env(as.list(list), parent = emptyenv(), hash = TRUE)
}

lookup_exists <- function(lookup, name) {
  !is.null(lookup[[name]])
}

lookup_set <- function(lookup, name) {
  lookup[[name]] <- TRUE
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
  tar_assert_true(all(unlist(as.list(lookup))))
}
