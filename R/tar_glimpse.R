#' @title Visualize an abridged fast dependency graph.
#' @export
#' @description Analyze the pipeline defined in `_targets.R`
#'   and visualize the directed acyclic graph of targets.
#'   Unlike [tar_visnetwork()], `tar_glimpse()` does not account for
#'   metadata or progress information, which means the graph
#'   renders faster. Also, `tar_glimpse()` omits functions and other global
#'   objects by default (but you can include them with `targets_only = FALSE`).
#' @return A `visNetwork` HTML widget object.
#' @inheritParams tar_validate
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
#' tar_glimpse()
#' tar_glimpse(allow = starts_with("y"))
#' })
#' }
tar_glimpse <- function(
  targets_only = TRUE,
  allow = NULL,
  exclude = NULL,
  callr_function = callr::r,
  callr_arguments = list()
) {
  assert_target_script()
  assert_package("visNetwork")
  assert_lgl(targets_only, "targets_only must be logical.")
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    targets_only = targets_only,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude)
  )
  callr_outer(
    targets_function = tar_glimpse_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments
  )
}

tar_glimpse_inner <- function(
  pipeline,
  targets_only,
  allow_quosure,
  exclude_quosure
) {
  pipeline_validate(pipeline)
  allow <- tar_tidyselect(allow_quosure, pipeline_get_names(pipeline))
  exclude <- tar_tidyselect(exclude_quosure, pipeline_get_names(pipeline))
  network <- glimpse_init(pipeline)
  visual <- visual_init(
    network = network,
    targets_only = targets_only,
    allow = allow,
    exclude = exclude
  )
  visual$update()
  visual$visnetwork
}
