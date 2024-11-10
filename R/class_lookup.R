lookup_new <- function(parent = emptyenv()) {
  new.env(parent = parent, hash = TRUE)
}

lookup_exists <- function(lookup, name) {
  !is.null(.subset2(lookup, name))
}

lookup_missing <- function(lookup, name) {
  is.null(.subset2(lookup, name))
}

lookup_get <- function(lookup, name) {
  .subset2(lookup, name)
}

lookup_list <- function(lookup) {
  as.character(names(lookup))
}

lookup_count <- function(lookup) {
  length(lookup_list(lookup))
}

lookup_set <- function(lookup, names, object) {
  for (name in names) {
    lookup[[name]] <- object
  }
}

lookup_unset <- function(lookup, names) {
  for (name in names) {
    lookup[[name]] <- NULL
  }
}

lookup_remove <- function(lookup, names) {
  remove(list = names, envir = lookup)
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
}
