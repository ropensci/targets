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
#' @inheritSection tar_target Storage formats
#' @inheritParams tar_target
#' @param imports Character vector of package names.
#'   For every package listed, `targets` tracks every
#'   dataset and every object in the package namespace
#'   as if it were part of the global namespace.
#'   As an example, say you have a package called `customAnalysisPackage`
#'   which contains an object called `analysis_function()`.
#'   If you write `tar_option_set(imports = "yourAnalysisPackage")` in your
#'   target script file (default: `_targets.R`),
#'   then a function called `"analysis_function"` will show up in the
#'   [tar_visnetwork()] graph, and any targets or functions
#'   referring to the symbol `"analysis_function"` will depend on the
#'   function `analysis_function()` from package `yourAnalysisPackage`.
#'   This is best combined with
#'   `tar_option_set(packages = "yourAnalysisPackage")` so
#'   that `analysis_function()` can actually be called in your code.
#'
#'   There are several important limitations:
#'     1. Namespaced calls, e.g. `yourAnalysisPackage::analysis_function()`,
#'        are ignored because of the limitations in `codetools::findGlobals()`
#'        which powers the static code analysis capabilities of `targets`.
#'     2. The `imports` option only looks at R objects and R code.
#'        It not account for low-level compiled code
#'        such as C/C++ or Fortran.
#'     3. If you supply multiple packages,
#'        e.g. `tar_option_set(imports = c("p1", "p2"))`, then the objects in
#'        `p1` override the objects in `p2` if there are name conflicts.
#'     4. Similarly, objects in `tar_option_get("envir")` override
#'        everything in `tar_option_get("imports")`.
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
#' @param backoff An object from [`tar_backoff()`] configuring the exponential
#'   backoff algorithm of the pipeline. See [`tar_backoff()`] for details.
#'   A numeric argument for `backoff` is still allowed, but deprecated.
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
#' @param seed Integer of length 1, seed for generating
#'   target-specific pseudo-random number generator seeds.
#'   These target-specific seeds are deterministic and depend on
#'   `tar_option_get("seed")` and the target name. Target-specific seeds
#'   are safely and reproducibly applied to each target's command,
#'   and they are stored in the metadata and retrievable with
#'   [tar_meta()] or [tar_seed()].
#'
#'   Either the user or third-party packages built on top of `targets`
#'   may still set seeds inside the command of a target.
#'   For example, some target factories in the
#'   `tarchetypes` package assigns replicate-specific
#'   seeds for the purposes of reproducible within-target batched replication.
#'   In cases like these, the effect of the target-specific seed saved
#'   in the metadata becomes irrelevant and the seed defined in the command
#'   applies.
#'
#'   The `seed` option can also be `NA` to disable
#'   automatic seed-setting. Any targets defined while
#'   `tar_option_get("seed")` is `NA` will not set a seed.
#'   In this case, those targets will never be up to date
#'   unless they have `cue = tar_cue(seed = FALSE)`.
#' @param controller A controller or controller group object
#'   produced by the `crew` R package. `crew` brings auto-scaled
#'   distributed computing to [tar_make()].
#' @param trust_object_timestamps Logical of length 1, whether to use
#'   file system modification timestamps to check whether the target output
#'   data files in `_targets/objects/` are up to date. This is an advanced
#'   setting and usually does not need to be set by the user
#'   except on old or difficult platforms.
#'
#'   If `trust_object_timestamps`
#'   is `TRUE` (default), then `targets` looks at the timestamp first.
#'   If it agrees with the timestamp recorded in the metadata, then `targets`
#'   considers the file unchanged. If the timestamps disagree, then `targets`
#'   recomputes the hash to make a final determination.
#'   This practice reduces the number of hash computations
#'   and thus saves time.
#'
#'   However, timestamp precision varies from a few
#'   nanoseconds at best to 2 entire seconds at worst, and timestamps
#'   with poor precision should not be fully trusted if there is any
#'   possibility that you will manually change the file within 2 seconds
#'   after the pipeline finishes.
#'   If the data store is on a file system with low-precision timestamps,
#'   then you may
#'   consider setting `trust_object_timestamps` to `FALSE` so `targets`
#'   errs on the safe side and always recomputes the hashes of files in
#'   `_targets/objects/`.
#'
#'   To check if your
#'   file system has low-precision timestamps, you can run
#'   `file.create("x"); nanonext::msleep(1); file.create("y");`
#'   from within the directory containing the `_targets` data store
#'   and then check
#'   `difftime(file.mtime("y"), file.mtime("x"), units = "secs")`.
#'   If the value from `difftime()` is around 0.001 seconds
#'   (must be strictly above 0 and below 1) then you do not need to set
#'   `trust_object_timestamps = FALSE`.
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset the format
#' tar_target(x, 1)$settings$format
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
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
  repository = NULL,
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
  workspace_on_error = NULL,
  seed = NULL,
  controller = NULL,
  trust_object_timestamps = NULL
) {
  force(envir)
  if_any(is.null(tidy_eval), NULL, tar_options$set_tidy_eval(tidy_eval))
  if_any(is.null(packages), NULL, tar_options$set_packages(packages))
  if_any(is.null(imports), NULL, tar_options$set_imports(imports))
  if_any(is.null(library), NULL, tar_options$set_library(library))
  if_any(is.null(envir), NULL, tar_options$set_envir(envir))
  if_any(is.null(format), NULL, tar_options$set_format(format))
  if_any(is.null(repository), NULL, tar_options$set_repository(repository))
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
  if_any(is.null(seed), NULL, tar_options$set_seed(seed))
  if_any(is.null(controller), NULL, tar_options$set_controller(controller))
  if_any(
    is.null(trust_object_timestamps),
    NULL,
    tar_options$set_trust_object_timestamps(trust_object_timestamps)
  )
  invisible()
}
