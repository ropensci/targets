#' @title `mermaid.js` dependency graph.
#' @export
#' @family visualize
#' @description Visualize the dependency graph with a static `mermaid.js` graph.
#' @details `mermaid.js` is a JavaScript library for constructing
#'   static visualizations of graphs.
#' @return A character vector of lines of code of the `mermaid.js` graph.
#'   You can visualize the graph by copying the text
#'   into a public online `mermaid.js` editor or a `mermaid` GitHub code chunk
#'   (`https://github.blog/2022-02-14-include-diagrams-markdown-files-mermaid/`). # nolint
#' @inheritParams tar_visnetwork
#' @param legend Logical of length 1, whether to display the legend.
#' @param color Logical of length 1, whether to color the graph vertices
#'   by status.
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
#' })
#' # Copy the text into a mermaid.js online editor
#' # or a mermaid GitHub code chunk:
#' tar_mermaid()
#' })
#' }
tar_mermaid <- function(
  targets_only = FALSE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = ".Random.seed",
  outdated = TRUE,
  label = NULL,
  legend = TRUE,
  color = TRUE,
  reporter = targets::tar_config_get("reporter_outdated"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  force(envir)
  tar_assert_lgl(targets_only, "targets_only must be logical.")
  tar_assert_lgl(outdated, "outdated in tar_mermaid() must be logical.")
  tar_assert_in(label, c("time", "size", "branches"))
  tar_assert_lgl(legend)
  tar_assert_lgl(color)
  tar_assert_scalar(legend)
  tar_assert_scalar(color)
  tar_config_assert_reporter_outdated(reporter)
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
    legend = legend,
    color = color,
    reporter = reporter
  )
  callr_outer(
    targets_function = tar_mermaid_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_mermaid"
  )
}

tar_mermaid_inner <- function(
  pipeline,
  path_store,
  targets_only,
  names_quosure,
  shortcut,
  allow_quosure,
  exclude_quosure,
  outdated,
  label,
  legend,
  color,
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
  visual <- mermaid_init(
    network = network,
    label = label,
    show_legend = legend,
    show_color = color
  )
  visual$update()
  visual$visual
}
