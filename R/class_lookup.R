lookup_new <- function() {
  new.env(parent = emptyenv(), hash = TRUE)
}

lookup_exists <- function(lookup, name) {
  !is.null(.subset2(lookup, name))
}

lookup_true <- function(lookup, name) {
  .subset2(lookup, name)
}

lookup_list <- function(lookup) {
  as.character(names(lookup))
}

lookup_set <- function(lookup, names, value) {
  for (name in names) {
    lookup[[name]] <- value
  }
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
  tar_assert_true(is.logical(unlist(as.list(lookup))))
}
