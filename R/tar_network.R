#' @title Return the vertices and edges of a pipeline dependency graph.
#' @export
#' @family inspect
#' @description Analyze the pipeline defined in the target script file
#'   (default: `_targets.R`)
#'   and return the vertices and edges of the directed acyclic graph
#'   of dependency relationships.
#' @return A list with two data frames: `vertices` and `edges`. The
#'   vertices data frame has one row per target with fields to denote
#'   the type of the target or object (stem, branch, map, cross, function,
#'   or object) and the target's status
#'   (up to date, outdated, started, canceled, or errored).
#'   The edges data frame has one row for every edge and columns `to` and
#'   `from` to mark the starting and terminating vertices.
#' @inheritParams tar_outdated
#' @param targets_only Logical, whether to restrict the output to just targets
#'   (`FALSE`) or to also include imported global functions and objects.
#' @param names Names of targets. The graph visualization will operate
#'   only on these targets (and unless `shortcut` is `TRUE`,
#'   all the targets upstream as well). Selecting a small subgraph
#'   using `names` could speed up the load time of the visualization.
#'   Unlike `allow`, `names` is invoked before the graph
#'   is generated.
#'   Set to NULL to check/build all the targets (default).
#'   Otherwise, you can supply symbols or tidyselect helpers
#'   like starts_with().
#'   Applies to ordinary targets (stem) and whole dynamic branching
#'   targets (patterns) but not individual dynamic branches.
#' @param allow Optional, define the set of allowable vertices in the graph.
#'   Unlike `names`, `allow` is invoked only after the graph is mostly
#'   resolved, so it will not speed up execution.
#'   Set to `NULL` to allow all vertices in the pipeline and environment
#'   (default). Otherwise, you can supply symbols or
#'   `tidyselect` helpers like [starts_with()].
#' @param exclude Optional, define the set of exclude vertices from the graph.
#'   Unlike `names`, `exclude` is invoked only after the graph is mostly
#'   resolved, so it will not speed up execution.
#'   Set to `NULL` to exclude no vertices.
#'   Otherwise, you can supply symbols or `tidyselect`
#'   helpers like [all_of()] and [starts_with()].
#' @param outdated Logical, whether to show colors to distinguish outdated
#'   targets from up-to-date targets. (Global functions and objects
#'   still show these colors.) Looking for outdated targets
#'   takes a lot of time for large pipelines with lots of branches,
#'   and setting `outdated` to `FALSE` is a nice way to speed up the graph
#'   if you only want to see dependency relationships and build progress.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_network(targets_only = TRUE)
#' })
#' }
tar_network <- function(
  targets_only = FALSE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = NULL,
  outdated = TRUE,
  reporter = targets::tar_config_get("reporter_outdated"),
  callr_function = callr::r,
  callr_arguments = targets::callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_assert_lgl(targets_only)
  tar_config_assert_reporter_outdated(reporter)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  targets_arguments <- list(
    path_store = store,
    targets_only = targets_only,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    outdated = outdated,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_network_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_network"
  )
}

tar_network_inner <- function(
  pipeline,
  path_store,
  targets_only,
  names_quosure,
  shortcut,
  allow_quosure,
  exclude_quosure,
  outdated,
  reporter
) {
  meta <- meta_init(path_store = path_store)
  progress <- progress_init(path_store = path_store)
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  inspection <- inspection_init(
    pipeline = pipeline,
    meta = meta,
    progress = progress,
    targets_only = targets_only,
    names = names,
    shortcut = shortcut,
    allow = allow_quosure,
    exclude = exclude_quosure,
    outdated = outdated,
    reporter = reporter
  )
  inspection$update()
  list(
    vertices = tibble::as_tibble(inspection$vertices),
    edges = tibble::as_tibble(inspection$edges)
  )
}
