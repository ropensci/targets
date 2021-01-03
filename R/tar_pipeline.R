#' @title Declare a pipeline (deprecated).
#' @export
#' @keywords internal
#' @description Functions `tar_pipeline()` and [tar_bind()] are deprecated.
#'   Instead, simply end your `_targets.R` file with a list of target objects.
#'   You can nest these objects however you like.
#' @details Deprecated on 2021-01-03.
#' @return A pipeline object.
#' @param ... Targets or lists of targets defined with [tar_target()].
#' @examples
#' # In _targets.R:
#' library(targets)
#' list( # You no longer need tar_pipeline() here.
#'   tar_target(data_file, "data.csv", format = "file"),
#'   list( # Target lists can be arbitrarily nested.
#'     tar_target(data_object, read.csv(data_file)),
#'     tar_target(analysis, analyze(data_object))
#'   )
#' )
tar_pipeline <- function(...) {
  warn_deprecate(
    "tar_pipeline() is deprecated in targets version >= 0.0.0.9004. ",
    "Simply end your _targets.R file ",
    "with a list of tar_target() objects (arbitrarily nested)."
  )
  pipeline_init(unlist(list(...), recursive = TRUE))
}
