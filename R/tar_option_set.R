#' @title Set target options.
#' @export
#' @family configuration
#' @description Set target options, including default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue. Only the non-null arguments are actually
#'   set as options. See currently set options with [tar_option_get()].
#'   To use `tar_option_set()` effectively, put it in your workflow's
#'   target script file (default: `_targets.R`)
#'   before calls to [tar_target()] or [tar_target_raw()].
#' @return `NULL` (invisibly).
#' @inheritParams tar_target
#' @param imports Character vector of package names to track
#'   global dependencies. For example, if you write
#'   `tar_option_set(imports = "yourAnalysisPackage")` early in your
#'   target script file (default: `_targets.R`)
#'   then `tar_make()` will automatically rerun or skip targets
#'   in response to changes to the R functions and objects defined in
#'   `yourAnalysisPackage`. Does not account for low-level compiled code
#'   such as C/C++ or Fortran. If you supply multiple packages,
#'   e.g. `tar_option_set(imports = c("p1", "p2"))`, then the objects in
#'   `p1` override the objects in `p2` if there are name conflicts.
#'   Similarly, objects in `tar_option_get("envir")` override
#'   everything in `tar_option_get("imports")`.
#' @param envir Environment containing functions and global objects
#'   common to all targets in the pipeline.
#'   The `envir` argument of [tar_make()] and related functions
#'   always overrides the current value of `tar_option_get("envir")`
#'   in the current R session just before running the target script file,
#'   so whenever you need to set an alternative `envir`, you should always set
#'   it with `tar_option_set()` from within the target script file.
#'   In other words, if you call `tar_option_set(envir = envir1)` in an
#'   interactive session and then
#'   `tar_make(envir = envir2, callr_function = NULL)`,
#'   then `envir2` will be used.
#'
#'   If `envir` is the global environment, all the promise objects
#'   are diffused before sending the data to parallel workers
#'   in [tar_make_future()] and [tar_make_clustermq()],
#'   but otherwise the environment is unmodified.
#'   This behavior improves performance by decreasing
#'   the size of data sent to workers.
#'
#'   If `envir` is not the global environment, then it should at least inherit
#'   from the global environment or base environment
#'   so `targets` can access attached packages.
#'   In the case of a non-global `envir`, `targets` attempts to remove
#'   potentially high memory objects that come directly from `targets`.
#'   That includes `tar_target()` objects of class `"tar_target"`,
#'   as well as objects of class `"tar_pipeline"` or `"tar_algorithm"`.
#'   This behavior improves performance by decreasing
#'   the size of data sent to workers.
#'
#'   Package environments should not be assigned to `envir`.
#'   To include package objects as upstream dependencies in the pipeline,
#'   assign the package to the `packages` and `imports` arguments
#'   of `tar_option_set()`.
#' @param backoff Numeric of length 1, must be greater than or equal to 0.01.
#'   Maximum upper bound of the random polling interval
#'   for the priority queue (seconds).
#'   In high-performance computing (e.g. [tar_make_clustermq()]
#'   and [tar_make_future()]) it can be expensive to repeatedly poll the
#'   priority queue if no targets are ready to process. The number of seconds
#'   between polls is `runif(1, 0.001, max(backoff, 0.001 * 1.5 ^ index))`,
#'   where `index` is the number of consecutive polls so far that found
#'   no targets ready to skip or run.
#'   (If no target is ready, `index` goes up by 1. If a target is ready,
#'   `index` resets to 0. For more information on exponential,
#'   backoff, visit <https://en.wikipedia.org/wiki/Exponential_backoff>).
#'   Raising `backoff` is kinder to the CPU etc. but may incur delays
#'   in some instances.
#' @param debug Character vector of names of targets to run in debug mode.
#'   To use effectively, you must set `callr_function = NULL` and
#'   restart your R session just before running. You should also
#'   [tar_make()], [tar_make_clustermq()], or [tar_make_future()].
#'   For any target mentioned in `debug`, `targets` will force the target to
#'   build locally (with `tar_cue(mode = "always")` and `deployment = "main"`
#'   in the settings) and pause in an interactive debugger to help you diagnose
#'   problems. This is like inserting a `browser()` statement at the
#'   beginning of the target's expression, but without invalidating any
#'   targets.
#' @param workspaces Character vector of target names.
#'   Could be non-branching targets, whole dynamic branching targets,
#'   or individual branch names. [tar_make()] and friends
#'   will save workspace files for these targets even if
#'   the targets are skipped. Workspace files help with debugging.
#'   See [tar_workspace()] for details about workspaces.
#' @param workspace_on_error Logical of length 1, whether to save
#'   a workspace file for each target that throws an error.
#'   Workspace files help with debugging.
#'   See [tar_workspace()] for details about workspaces.
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset the format
#' tar_target(x, 1)$settings$format
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set(cue = tar_cue(mode = "always")) # All targets always run.
#'   list(tar_target(x, 1), tar_target(y, 2))
#' })
#' tar_make()
#' tar_make()
#' })
#' }
tar_option_set <- function(
  tidy_eval = NULL,
  packages = NULL,
  imports = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  garbage_collection = NULL,
  deployment = NULL,
  priority = NULL,
  backoff = NULL,
  resources = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL,
  workspaces = NULL,
  workspace_on_error = NULL
) {
  force(envir)
  if_any(is.null(tidy_eval), NULL, tar_options$set_tidy_eval(tidy_eval))
  if_any(is.null(packages), NULL, tar_options$set_packages(packages))
  if_any(is.null(imports), NULL, tar_options$set_imports(imports))
  if_any(is.null(library), NULL, tar_options$set_library(library))
  if_any(is.null(envir), NULL, tar_options$set_envir(envir))
  if_any(is.null(format), NULL, tar_options$set_format(format))
  if_any(is.null(iteration), NULL, tar_options$set_iteration(iteration))
  if_any(is.null(error), NULL, tar_options$set_error(error))
  if_any(is.null(memory), NULL, tar_options$set_memory(memory))
  if_any(
    is.null(garbage_collection),
    NULL,
    tar_options$set_garbage_collection(garbage_collection)
  )
  if_any(is.null(deployment), NULL, tar_options$set_deployment(deployment))
  if_any(is.null(priority), NULL, tar_options$set_priority(priority))
  if_any(is.null(backoff), NULL, tar_options$set_backoff(backoff))
  if_any(is.null(resources), NULL, tar_options$set_resources(resources))
  if_any(is.null(storage), NULL, tar_options$set_storage(storage))
  if_any(is.null(retrieval), NULL, tar_options$set_retrieval(retrieval))
  if_any(is.null(cue), NULL, tar_options$set_cue(cue))
  if_any(is.null(debug), NULL, tar_options$set_debug(debug))
  if_any(is.null(workspaces), NULL, tar_options$set_workspaces(workspaces))
  if_any(
    is.null(workspace_on_error),
    NULL,
    tar_options$set_workspace_on_error(workspace_on_error)
  )
  invisible()
}
