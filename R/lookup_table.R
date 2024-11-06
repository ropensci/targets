lookup_table_new <- function() {
  new.env(parent = emptyenv(), hash = TRUE)
}

lookup_table_exists <- function(lookup_table, category, name) {
  lookup_exists(lookup = .subset2(lookup_table, category), name = name)
}

lookup_table_exists_category <- function(lookup_table, category) {
  !is.null(.subset2(lookup_table, category))
}

lookup_table_true <- function(lookup_table, category, name) {
  lookup_true(lookup = .subset2(lookup_table, category), name = name)
}

lookup_table_list <- function(lookup_table, category) {
  lookup_list(lookup = .subset2(lookup_table, category))
}

lookup_table_set <- function(lookup_table, category, names, value) {
  lookup <- .subset2(lookup_table, category)
  if (is.null(lookup)) {
    lookup_table[[category]] <- lookup_new()
    lookup <- .subset2(lookup_table, category)
  }
  lookup_set(lookup = lookup, names = names, value = value)
}

lookup_table_get_lookup(lookup_table, category) {
  .subset2(lookup_table, category)
}

lookup_table_validate <- function(lookup_table) {
  tar_assert_envir(lookup_table)
  for (category in names(lookup_table)) {
    lookup_validate(lookup_table_get_lookup(lookup_table, category))
  }
}
