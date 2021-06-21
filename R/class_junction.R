junction_init <- function(
  nexus = character(0),
  splits = character(0),
  deps = list()
) {
  splits <- make.unique(splits, sep = "_")
  names(deps) <- names(deps) %|||% seq_along(deps)
  deps <- as_data_frame(deps)
  junction_new(nexus, splits, deps)
}

junction_new <- function(nexus = NULL, splits = NULL, deps = NULL) {
  force(nexus)
  force(splits)
  force(deps)
  environment()
}

junction_upstream_edges <- function(junction) {
  from <- utils::stack(junction$deps)$values
  to <- rep(junction$splits, times = ncol(junction$deps))
  data_frame(from = from, to = to)
}

junction_transpose <- function(junction) {
  splits <- junction$splits
  deps <- junction$deps
  out <- map_rows(deps, ~list(deps = unname(.x))) %||%
    replicate(length(splits), list(deps = character(0)), simplify = FALSE)
  for (index in seq_along(splits)) {
    out[[index]]$split <- splits[index]
    out[[index]]$index <- index
  }
  out
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
  tar_assert_chr(junction$splits)
  junction_validate_deps(junction$deps)
}
