#' @title Visualize an abridged fast dependency graph.
#' @export
#' @description Analyze the pipeline defined in `_targets.R`
#'   and visualize the directed acyclic graph of targets
#'   and imported global functions and objects.
#' @return A `visNetwork` HTML widget object.
#' @inheritParams tar_validate
#' @inheritParams tar_outdated
#' @param targets_only Logical, whether to restrict the output to just targets
#'   (`FALSE`) or to also include imported global functions and objects.
#' @param allow Optional, define the set of allowable vertices in the graph.
#'   Set to `NULL` to allow all vertices in the pipeline and environment
#'   (default). Otherwise, you can supply symbols, a character vector, or
#'   `tidyselect` helpers like [starts_with()].
#' @param exclude Optional, define the set of exclude vertices from the graph.
#'   Set to `NULL` to exclude no vertices.
#'   Otherwise, you can supply symbols, a character vector, or `tidyselect`
#'   helpers like [starts_with()].
#' @param outdated Logical, whether to show colors to distinguish outdated
#'   targets from up-to-date targets. (Global functions and objects
#'   still show these colors.) Looking for outdated targets
#'   takes a lot of time for large pipelines with lots of branches,
#'   and setting `outdated` to `FALSE` is a nice way to speed up the graph
#'   if you only want to see dependency relationships and build progress.
#' @param label Character vector of one or more aesthetics to add to the
#'   vertex labels. Can contain `"time"` to show total runtime, `"size"`
#'   to show total storage size, or `"branches"` to show the number of
#'   branches in each pattern. You can choose multiple aesthetics
#'   at once, e.g. `label = c("time", "branches")`. All are disabled
#'   by default because they clutter the graph.
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script({
#'   tar_option_set()
#'   tar_pipeline(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' })
#' tar_visnetwork()
#' tar_visnetwork(allow = starts_with("y"))
#' })
#' }
tar_visnetwork <- function(
  targets_only = FALSE,
  allow = NULL,
  exclude = NULL,
  outdated = TRUE,
  label = NULL,
  reporter = "silent",
  callr_function = callr::r,
  callr_arguments = list(spinner = reporter == "silent")
) {
  assert_target_script()
  assert_package("visNetwork")
  assert_lgl(targets_only, "targets_only must be logical.")
  assert_lgl(outdated, "outdated in tar_visnetwork() must be logical.")
  assert_in(label, c("time", "size", "branches"))
  assert_in(
    reporter,
    c("forecast", "silent"),
    "reporter arg of tar_outdated() must either be \"silent\" or \"forecast\""
  )
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    targets_only = targets_only,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    outdated = outdated,
    label = label,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_visnetwork_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
}

tar_visnetwork_inner <- function(
  pipeline,
  targets_only,
  allow_quosure,
  exclude_quosure,
  outdated,
  label,
  reporter
) {
  pipeline_validate_lite(pipeline)
  allow <- eval_tidyselect(allow_quosure, pipeline_get_names(pipeline))
  exclude <- eval_tidyselect(exclude_quosure, pipeline_get_names(pipeline))
  network <- inspection_init(
    pipeline,
    outdated = outdated,
    reporter = reporter
  )
  visual <- visual_init(
    subclass = "visnetwork",
    network = network,
    targets_only = targets_only,
    allow = allow,
    exclude = exclude,
    label = label
  )
  visual$update()
  visual$visnetwork
}
