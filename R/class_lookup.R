lookup_init <- function(list, parent = emptyenv()) {
  list2env(x = list, parent = parent, hash = TRUE)
}

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
  index <- 1L
  n <- length(names)
  while (index <= n) {
    lookup[[.subset(names, index)]] <- object
    index <- index + 1L
  }
}

lookup_unset <- function(lookup, names) {
  index <- 1L
  n <- length(names)
  while (index <= n) {
    lookup[[.subset(names, index)]] <- NULL
    index <- index + 1L
  }
}

lookup_remove <- function(lookup, names) {
  remove(list = names, envir = lookup)
}

lookup_validate <- function(lookup) {
  tar_assert_envir(lookup)
}
