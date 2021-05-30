#' @title Visualize an abridged fast dependency graph.
#' @export
#' @family inspect
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
#' @param level_separation Numeric of length 1,
#'   `levelSeparation` argument of `visNetwork::visHierarchicalLayout()`.
#'   Controls the distance between hierarchical levels.
#'   Consider changing the value if the aspect ratio of the graph
#'   is far from 1. If `level_separation` is `NULL`,
#'   the `levelSeparation` argument of `visHierarchicalLayout()`
#'   defaults to `150`.
#' @param degree_from Integer of length 1. When you click on a node,
#'   the graph highlights a neighborhood of that node. `degree_from`
#'   controls the number of edges the neighborhood extends upstream.
#' @param degree_to Integer of length 1. When you click on a node,
#'   the graph highlights a neighborhood of that node. `degree_to`
#'   controls the number of edges the neighborhood extends downstream.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_glimpse()
#' tar_glimpse(allow = starts_with("y"))
#' })
#' }
tar_glimpse <- function(
  targets_only = TRUE,
  allow = NULL,
  exclude = ".Random.seed",
  level_separation = NULL,
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function),
  degree_from = 1L,
  degree_to = 1L
) {
  assert_script()
  assert_package("visNetwork")
  assert_lgl(targets_only, "targets_only must be logical.")
  assert_scalar(degree_from, "degree_from must have length 1.")
  assert_scalar(degree_to, "degree_to must have length 1.")
  assert_dbl(degree_from, "degree_from must be numeric.")
  assert_dbl(degree_to, "degree_to must be numeric.")
  assert_ge(degree_from, 0L, "degree_from must be at least 0.")
  assert_ge(degree_to, 0L, "degree_to must be at least 0.")
  assert_callr_function(callr_function)
  assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    targets_only = targets_only,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to
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
  exclude_quosure,
  level_separation,
  degree_from,
  degree_to
) {
  network <- glimpse_init(pipeline)
  visual <- visnetwork_init(
    network = network,
    targets_only = targets_only,
    allow = allow_quosure,
    exclude = exclude_quosure,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to
  )
  visual$update()
  visual$visnetwork
}
