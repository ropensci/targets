lookup_init <- function(true, false) {
  list <- c(rep(TRUE, length(true)), rep(FALSE, length(true)))
  names(list) <- c(true, false)
  list2env(as.list(list), parent = emptyenv(), hash = TRUE)
}

lookup_true <- function(lookup, name) {
  lookup[[name]]
}

lookup_exists <- function(lookup, name) {
  !is.null(lookup[[name]])
}

lookup_set <- function(lookup, name, value) {
  lookup[[name]] <- value
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
  tar_assert_true(is.logical(unlist(as.list(lookup))))
}
