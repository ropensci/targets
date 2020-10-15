#' @title Combine pipelines.
#' @export
#' @description Combine multiple pipeline objects
#'   into a single pipeline object.
#' @param ... Pipeline objects or nested lists of pipeline objects.
#'   You can generate a pipeline object with [tar_pipeline()].
#' @examples
#' # Include in _targets.R:
#' pipeline1 <- tar_pipeline(tar_target(x, 1))
#' pipeline2 <- tar_pipeline(tar_target(y, 2))
#' # Pipeline object with both targets:
#' pipeline <- tar_bind(pipeline1, pipeline2)
tar_bind <- function(...) {
  x <- unlist(list(...), recursive = TRUE)
  map(x, ~assert_inherits(.x, "tar_pipeline"))
  names <- unlist(map(x, pipeline_get_names))
  assert_unique_targets(names)
  targets <- map(x, ~as.list(.x$targets))
  tar_pipeline(targets)
}
