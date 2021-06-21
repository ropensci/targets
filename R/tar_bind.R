#' @title Combine pipeline objects (deprecated).
#' @export
#' @keywords internal
#' @description Functions `tar_pipeline()` and [tar_bind()] are deprecated.
#'   Instead, simply end your target script file
#'   (default: `_targets.R`) file with a list of target objects.
#'   You can nest these objects however you like.
#' @details Deprecated on 2021-01-03.
#' @param ... Pipeline objects or nested lists of pipeline objects.
#'   You can generate a pipeline object with [tar_pipeline()].
#' @examples
#' # In your target script file (default: _targets.R):
#' library(targets)
#' list( # You no longer need tar_pipeline() here.
#'   tar_target(data_file, "data.csv", format = "file"),
#'   list( # Target lists can be arbitrarily nested.
#'     tar_target(data_object, read.csv(data_file)),
#'     tar_target(analysis, analyze(data_object))
#'   )
#' )
tar_bind <- function(...) {
  tar_warn_deprecate(
    "tar_bind() and tar_pipeline() are deprecated ",
    "in targets version >= 0.0.0.9004. Simply end your target script file ",
    "with a list of tar_target() objects (arbitrarily nested)."
  )
  x <- unlist(list(...), recursive = TRUE)
  map(x, ~tar_assert_inherits(.x, "tar_pipeline"))
  names <- unlist(map(x, pipeline_get_names))
  tar_assert_unique_targets(names)
  targets <- map(x, ~as.list(.x$targets))
  tar_pipeline(targets)
}
