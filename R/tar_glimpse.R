#' @title Visualize an abridged fast dependency graph.
#' @export
#' @family visualize
#' @description Analyze the pipeline defined in the target script file
#'   (default: `_targets.R`)
#'   and visualize the directed acyclic graph of targets.
#'   Unlike [tar_visnetwork()], `tar_glimpse()` does not account for
#'   metadata or progress information, which means the graph
#'   renders faster. Also, `tar_glimpse()` omits functions and other global
#'   objects by default (but you can include them with `targets_only = FALSE`).
#' @inheritSection tar_network Dependency graph
#' @inheritSection tar_meta Storage access
#' @return A `visNetwork` HTML widget object.
#' @inheritParams tar_network
#' @param targets_only Logical, whether to restrict the output to just targets
#'   (`FALSE`) or to also include global functions and objects.
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
#' @param zoom_speed Positive numeric of length 1, scaling factor on the
#'   zoom speed. Above 1 zooms faster than default, below 1 zooms
#'   lower than default.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_glimpse()
#' tar_glimpse(allow = starts_with("y")) # see also any_of()
#' })
#' }
tar_glimpse <- function(
  targets_only = TRUE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = ".Random.seed",
  level_separation = targets::tar_config_get("level_separation"),
  degree_from = 1L,
  degree_to = 1L,
  zoom_speed = 1,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_glimpse")
  force(envir)
  tar_assert_package("visNetwork")
  tar_assert_lgl(targets_only)
  tar_assert_scalar(degree_from)
  tar_assert_scalar(degree_to)
  tar_assert_dbl(degree_from)
  tar_assert_dbl(degree_to)
  tar_assert_ge(degree_from, 0L)
  tar_assert_ge(degree_to, 0L)
  tar_assert_scalar(zoom_speed)
  tar_assert_dbl(zoom_speed)
  tar_assert_positive(zoom_speed)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  targets_arguments <- list(
    path_store = store,
    targets_only = targets_only,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed
  )
  callr_outer(
    targets_function = tar_glimpse_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_glimpse"
  )
}

tar_glimpse_inner <- function(
  pipeline,
  path_store,
  targets_only,
  names_quosure,
  shortcut,
  allow_quosure,
  exclude_quosure,
  level_separation,
  degree_from,
  degree_to,
  zoom_speed
) {
  meta <- meta_init(path_store = path_store)
  progress <- progress_init(path_store = path_store)
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  network <- glimpse_init(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    targets_only = targets_only,
    names = names,
    shortcut = shortcut,
    allow = allow_quosure,
    exclude = exclude_quosure
  )
  visual <- visnetwork_init(
    network = network,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed
  )
  visual$update()
  visual$visual
}
