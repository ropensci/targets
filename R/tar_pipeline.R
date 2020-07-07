#' @title Declare a pipeline of targets.
#' @export
#' @description A pipeline is similar to a `Makefile` or a `drake` plan.
#'   It is a collection of targets, or skippable steps,
#'   in an automated reproducible workflow.
#' @return A pipeline object. The `_targets.R` file of a project must
#'   end with a call `tar_pipeline()`.
#'   Otherwise, users do not work pipeline objects directly.
#' @param ... Targets or lists of targets defined with [tar_target()].
#' @examples
#' file <- tar_target(data_file, "data.csv", format = "file")
#' object <- tar_target(data_object, read.csv(data_file))
#' analysis <- tar_target(analysis, analyze(data_object))
#' pipeline <- tar_pipeline(file, object, analysis)
#' print(pipeline)
#' # Equivalent:
#' target_list <- list(
#'   tar_target(data_file, "data.csv", format = "file"),
#'   tar_target(data_object, read.csv(data_file)),
#'   tar_target(analysis, analyze(data_object))
#' )
#' pipeline <- tar_pipeline(target_list)
tar_pipeline <- function(...) {
  pipeline_init(unlist(list(...), recursive = TRUE))
}
