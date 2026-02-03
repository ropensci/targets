#' @title Get the igraph.
#' @export
#' @family inspect
#' @description Return an igraph object with the dependency graph
#'   of the pipeline.
#' @details If you have `igraph` >= 2.2.0 installed, then you can debug
#'   cycles in your pipeline by running `igraph::find_cycle(tar_igraph())`.
#'   This helps detect cycles in the dependency graph of the pipeline.
#'   A cycle happens when a target directly or indirectly depends on itself,
#'   which is not allowed. `igraph::find_cycle()` returns a character vector
#'   of the names of the targets and objects involved in the cycle
#'   (if a cycle exists).
#' @return An `igraph` object with the dependency graph.
#' @inheritParams tar_network
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#'   tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#'     tar_script(
#'       list(
#'         tar_target(x, 1),
#'         tar_target(y, 2)
#'       ),
#'       ask = FALSE
#'     )
#'     tar_igraph()
#'   })
#' }
tar_igraph <- function(
  targets_only = FALSE,
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  force(envir)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments, "callr_arguments mut be a list.")
  callr_outer(
    targets_function = tar_igraph_inner,
    targets_arguments = list(targets_only = targets_only),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = tempfile(),
    fun = "tar_igraph"
  )
}

tar_igraph_inner <- function(pipeline, targets_only) {
  pipeline_produce_igraph(pipeline, targets_only = targets_only)
}
