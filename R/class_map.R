map_new <- function(
  command = NULL,
  settings = NULL,
  cue = NULL,
  cache = NULL,
  value = NULL,
  junction = NULL,
  sitrep = NULL
) {
  force(command)
  force(settings)
  force(cue)
  force(cache)
  force(value)
  force(junction)
  force(sitrep)
  enclass(environment(), c("tar_map", "tar_pattern", "tar_target"))
}

map_assert_niblings <- function(niblings) {
  lengths <- map_int(niblings, length)
  if (length(unique(lengths)) > 1L) {
    throw_pattern("map() pattern needs equal-length nonempty variables.")
  }
}

#' @export
target_produce_junction.tar_map <- function(target, pipeline) {
  dimensions <- target$settings$dimensions
  pattern_assert_dimensions(target, dimensions, pipeline)
  siblings <- setdiff(target_deps_shallow(target, pipeline), dimensions)
  niblings <- pattern_get_nibling_list(dimensions, pipeline)
  map_assert_niblings(niblings)
  deps <- pattern_combine_niblings_siblings(niblings, siblings)
  names <- pattern_name_branches(target_get_parent(target), niblings)
  junction_init(target_get_parent(target), names, deps)
}

#' @export
target_get_type.tar_map <- function(target) {
  "map"
}

#' @export
print.tar_map <- function(x, ...) {
  expr <- x$command$expr
  cat(
    "<map target>",
    "\n  name:", target_get_name(x),
    "\n  command:\n   ",
    produce_lines(string_sub_expression(x$command$string)),
    "\n  maps over:\n   ",
    produce_lines(x$settings$dimensions),
    "\n  format:", x$settings$format,
    "\n  iteration method:", x$settings$iteration,
    "\n  error mode:", x$settings$error,
    "\n  memory mode:", x$settings$memory,
    "\n  storage mode:", x$settings$storage,
    "\n  retrieval mode:", x$settings$retrieval,
    "\n  deploy to:", x$settings$deployment,
    "\n  template (clustermq):\n   ",
    produce_lines(paste_list(x$settings$template)),
    "\n  resources (future):\n   ",
    produce_lines(paste_list(x$settings$resources)),
    "\n  cue:\n   ",
    produce_lines(paste_list(as.list(x$cue))),
    "\n  packages:\n   ", produce_lines(sort(x$command$packages)),
    "\n  library:\n   ", produce_lines(sort(x$command$library))
  )
}
