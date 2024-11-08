junction_init <- function(
  nexus = character(0),
  splits = character(0),
  deps = list()
) {
  index <- seq_along(splits)
  names(index) <- make.unique(splits, sep = "_")
  names(deps) <- names(deps) %|||% seq_along(deps)
  deps <- as_data_frame(deps)
  junction_new(nexus, index, deps)
}

junction_new <- function(nexus = NULL, index = NULL, deps = NULL) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$nexus <- nexus
  out$index <- index
  out$deps <- deps
  out
}

junction_upstream_edges <- function(junction) {
  from <- utils::stack(junction$deps)$values
  to <- rep(junction_splits(junction), times = ncol(junction$deps))
  data_frame(from = from, to = to)
}

junction_splits <- function(junction) {
  names(junction$index)
}

junction_transpose <- function(junction) {
  splits <- junction_splits(junction)
  deps <- junction$deps
  out <- map_rows(deps, ~list(deps = unname(.x))) %||%
    replicate(length(splits), list(deps = character(0)), simplify = FALSE)
  for (index in seq_along(splits)) {
    out[[index]]$split <- splits[index]
  }
  out
}

junction_invalidate <- function(junction) {
  names(junction$index) <- rep(NA_character_, length(junction$index))
}

junction_validate_deps <- function(deps) {
  if (!is.null(deps) && !is.data.frame(deps)) {
    tar_throw_validate("deps field of junction must be null or a data frame.")
  }
}

junction_validate <- function(junction) {
  tar_assert_correct_fields(junction, junction_new)
  tar_assert_scalar(junction$nexus)
  tar_assert_chr(junction$nexus)
  tar_assert_int(junction$index)
  tar_assert_chr(junction_splits(junction))
  junction_validate_deps(junction$deps)
}
