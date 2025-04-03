junction_init <- function(
  nexus = character(0),
  splits = character(0),
  deps = list()
) {
  splits <- make.unique(splits, sep = "_")
  deps <- as_data_frame(deps)
  has_deps <- nrow(deps) > 0L
  junction_new(nexus, splits, deps, has_deps)
}

junction_new <- function(
  nexus = NULL,
  splits = NULL,
  deps = NULL,
  has_deps = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$nexus <- nexus
  out$splits <- splits
  out$deps <- deps
  out$has_deps <- has_deps
  out
}

junction_upstream_edges <- function(junction) {
  from <- unlist(junction$deps, use.names = FALSE)
  to <- rep(junction_splits(junction), times = ncol(junction$deps))
  data_frame(from = from, to = to)
}

junction_length <- function(junction) {
  length(.subset2(junction, "splits"))
}

junction_splits <- function(junction) {
  .subset2(junction, "splits")
}

junction_extract_deps <- function(junction, index) {
  if (.subset2(junction, "has_deps")) {
    deps <- .subset2(junction, "deps")
    slice <- vctrs::vec_slice(x = deps, i = index)
    unlist(as.list(slice), use.names = FALSE)
  } else {
    character(0L)
  }
}

junction_invalidate <- function(junction) {
  junction$splits <- rep(NA_character_, junction_length(junction))
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
