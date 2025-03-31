#' @title visNetwork dependency graph.
#' @export
#' @family visualize
#' @description Visualize the pipeline dependency graph with a `visNetwork`
#'   HTML widget.
#' @inheritSection tar_network Dependency graph
#' @inheritSection tar_meta Storage access
#' @return A `visNetwork` HTML widget object.
#' @inheritParams tar_glimpse
#' @inheritParams tar_network
#' @param label Character vector of one or more aesthetics to add to the
#'   vertex labels. Can contain `"description"` to show each
#'   target's custom description, `"time"` to show total runtime, `"size"`
#'   to show total storage size, or `"branches"` to show the number of
#'   branches in each pattern. You can choose multiple aesthetics
#'   at once, e.g. `label = c("description", "time")`.
#'   Only the description is enabled by default.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set()
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2, description = "sum of two other sums")
#'   )
#' })
#' tar_visnetwork()
#' tar_visnetwork(allow = starts_with("y")) # see also any_of()
#' })
#' }
tar_visnetwork <- function(
  targets_only = FALSE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = ".Random.seed",
  outdated = TRUE,
  label = targets::tar_config_get("label"),
  label_width = targets::tar_config_get("label_width"),
  level_separation = targets::tar_config_get("level_separation"),
  degree_from = 1L,
  degree_to = 1L,
  zoom_speed = 1,
  physics = FALSE,
  reporter = targets::tar_config_get("reporter_outdated"),
  seconds_reporter = targets::tar_config_get("seconds_reporter_outdated"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_visnetwork", store)
  force(envir)
  tar_assert_package("visNetwork")
  tar_assert_lgl(targets_only, "targets_only must be logical.")
  tar_assert_lgl(outdated, "outdated in tar_visnetwork() must be logical.")
  tar_assert_in(label, c("description", "time", "size", "branches"))
  tar_assert_dbl(label_width)
  tar_assert_scalar(label_width)
  tar_assert_none_na(label_width)
  tar_assert_ge(label_width, 1L)
  tar_assert_scalar(degree_from, "degree_from must have length 1.")
  tar_assert_scalar(degree_to, "degree_to must have length 1.")
  tar_assert_dbl(degree_from, "degree_from must be numeric.")
  tar_assert_dbl(degree_to, "degree_to must be numeric.")
  tar_assert_ge(degree_from, 0L, "degree_from must be at least 0.")
  tar_assert_ge(degree_to, 0L, "degree_to must be at least 0.")
  tar_assert_scalar(zoom_speed)
  tar_assert_dbl(zoom_speed)
  tar_assert_positive(zoom_speed)
  tar_assert_lgl(physics)
  tar_assert_scalar(physics)
  reporter <- tar_outdated_reporter(reporter)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments, "callr_arguments mut be a list.")
  targets_arguments <- list(
    path_store = store,
    targets_only = targets_only,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    outdated = outdated,
    label = label,
    label_width = label_width,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed,
    physics = physics,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_visnetwork_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_visnetwork"
  )
}

tar_visnetwork_inner <- function(
  pipeline,
  path_store,
  targets_only,
  names_quosure,
  shortcut,
  allow_quosure,
  exclude_quosure,
  outdated,
  label,
  label_width,
  level_separation,
  degree_from,
  degree_to,
  zoom_speed,
  physics,
  reporter
) {
  names <- tar_tidyselect_eval(names_quosure, pipeline_get_names(pipeline))
  network <- inspection_init(
    pipeline,
    meta = meta_init(path_store = path_store),
    progress = progress_init(path_store = path_store),
    targets_only = targets_only,
    names = names,
    shortcut = shortcut,
    allow = allow_quosure,
    exclude = exclude_quosure,
    outdated = outdated,
    reporter = reporter
  )
  visual <- visnetwork_init(
    network = network,
    label = label,
    label_width = label_width,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    zoom_speed = zoom_speed,
    physics = physics
  )
  visual$update()
  visual$visual
}
