#' @title Set target options.
#' @export
#' @description Set target options, including default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue. See default options with [tar_option_get()].
#'   To use `tar_option_set()` effectively, put it in your workflow's
#'   `_targets.R` script before calls to [tar_target()] or [tar_target_raw()].
#' @return Nothing.
#' @inheritParams tar_target
#' @param imports Character vector of package names to track
#'   global dependencies. For example, if you write
#'   `tar_option_set(imports = "yourAnalysisPackage")` early in `_targets.R`,
#'   then `tar_make()` will automatically rerun or skip targets
#'   in response to changes to the R functions and objects defined in
#'   `yourAnalysisPackage`. Does not account for low-level compiled code
#'   such as C/C++ or Fortran. If you supply multiple packages,
#'   e.g. `tar_option_set(imports = c("p1", "p2"))`, then the objects in
#'   `p1` override the objects in `p2` if there are name conflicts.
#'   Similarly, objects in `tar_option_get("envir")` override
#'   everything in `tar_option_get("imports")`.
#' @param envir Environment containing functions and global objects
#'   used in the R commands to run targets.
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
#' @param workspaces Character vector of names of targets to save workspace
#'   files. Workspace files let you re-create a target's runtime environment
#'   in an interactive R session using [tar_workspace()]. [tar_workspace()]
#'   loads a target's random number generator seed and dependency objects
#'   as long as those target objects are still in the data store
#'   (usually `_targets/objects/`).
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset the format
#' tar_target(x, 1)$settings$format
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
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
  resources = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL,
  workspaces = NULL
) {
  force(envir)
  tar_option_set_tidy_eval(tidy_eval)
  tar_option_set_packages(packages)
  tar_option_set_imports(imports)
  tar_option_set_library(library)
  tar_option_set_envir(envir)
  tar_option_set_format(format)
  tar_option_set_iteration(iteration)
  tar_option_set_error(error)
  tar_option_set_memory(memory)
  tar_option_set_garbage_collection(garbage_collection)
  tar_option_set_deployment(deployment)
  tar_option_set_priority(priority)
  tar_option_set_resources(resources)
  tar_option_set_storage(storage)
  tar_option_set_retrieval(retrieval)
  tar_option_set_cue(cue)
  tar_option_set_debug(debug)
  tar_option_set_workspaces(workspaces)
}

tar_option_set_tidy_eval <- function(tidy_eval) {
  tidy_eval <- tidy_eval %||% tar_option_get("tidy_eval")
  assert_lgl(tidy_eval, "tidy_eval in tar_option_set() must be logical.")
  assign("tidy_eval", tidy_eval, envir = tar_envir_options)
}

tar_option_set_packages <- function(packages) {
  packages <- packages %||% tar_option_get("packages")
  assert_chr(packages, "packages in tar_option_set() must be character.")
  assign("packages", packages, envir = tar_envir_options)
}

tar_option_set_imports <- function(imports) {
  imports <- imports %||% tar_option_get("imports")
  assert_chr(imports, "imports in tar_option_set() must be character.")
  assign("imports", imports, envir = tar_envir_options)
}

tar_option_set_library <- function(library) {
  library <- library %||% tar_option_get("library")
  assert_chr(library %||% character(0), "library must be NULL or character.")
  assign("library", library, envir = tar_envir_options)
}

tar_option_set_envir <- function(envir) {
  envir <- envir %||% tar_option_get("envir")
  msg <- paste(
    "envir in tar_option_set() must be the environment",
    "where you put your functions and global objects",
    "(global environment for most users)."
  )
  assert_envir(envir, msg)
  assign("envir", envir, envir = tar_envir_options)
}

tar_option_set_format <- function(format) {
  format <- format %||% tar_option_get("format")
  assert_format(format)
  assign("format", format, envir = tar_envir_options)
}

tar_option_set_iteration <- function(iteration) {
  iteration <- iteration %||% tar_option_get("iteration")
  assert_flag(iteration, c("vector", "list", "group"))
  assign("iteration", iteration, envir = tar_envir_options)
}

tar_option_set_error <- function(error) {
  error <- error %||% tar_option_get("error")
  assert_flag(error, c("stop", "continue", "workspace"))
  assign("error", error, envir = tar_envir_options)
}

tar_option_set_memory <- function(memory) {
  memory <- memory %||% tar_option_get("memory")
  assert_flag(memory, c("persistent", "transient"))
  assign("memory", memory, envir = tar_envir_options)
}

tar_option_set_garbage_collection <- function(garbage_collection) {
  garbage_collection <- garbage_collection %||%
    tar_option_get("garbage_collection")
  garbage_collection <- as.logical(garbage_collection)
  assert_lgl(garbage_collection, "garbage_collection must be logical.")
  assert_scalar(garbage_collection, "garbage_collection must be a scalar.")
  assign("garbage_collection", garbage_collection, envir = tar_envir_options)
}

tar_option_set_deployment <- function(deployment) {
  deployment <- deployment %||% tar_option_get("deployment")
  assert_flag(deployment, c("worker", "main"))
  assign("deployment", deployment, envir = tar_envir_options)
}

tar_option_set_priority <- function(priority) {
  priority <- priority %||% tar_option_get("priority")
  assert_dbl(priority, msg = "priority must be numeric")
  assert_scalar(priority, msg = "priority must have length 1")
  assert_ge(priority, 0, msg = "priority cannot be less than 0")
  assert_le(priority, 1, msg = "priority cannot be greater than 1")
  assign("priority", priority, envir = tar_envir_options)
}

tar_option_set_resources <- function(resources) {
  resources <- resources %||% tar_option_get("resources")
  assert_list(resources, "resources in tar_option_set() must be a named list.")
  assign("resources", resources, envir = tar_envir_options)
}

tar_option_set_storage <- function(storage) {
  storage <- storage %||% tar_option_get("storage")
  assert_flag(storage, c("main", "worker"))
  assign("storage", storage, envir = tar_envir_options)
}

tar_option_set_retrieval <- function(retrieval) {
  retrieval <- retrieval %||% tar_option_get("retrieval")
  assert_flag(retrieval, c("main", "worker"))
  assign("retrieval", retrieval, envir = tar_envir_options)
}

tar_option_set_cue <- function(cue) {
  cue <- cue %||% tar_option_get("cue")
  trn(is.null(cue), NULL, cue_validate(cue))
  assign("cue", cue, envir = tar_envir_options)
}

tar_option_set_debug <- function(debug) {
  debug <- debug %||% tar_option_get("debug")
  assert_chr(debug, "debug argument of tar_option_set() must be a character.")
  assign("debug", debug, envir = tar_envir_options)
}

tar_option_set_workspaces <- function(workspaces) {
  workspaces <- workspaces %||% tar_option_get("workspaces")
  assert_chr(
    workspaces,
    "workspaces argument of tar_option_set() must be a character."
  )
  assign("workspaces", workspaces, envir = tar_envir_options)
}

tar_envir_options <- new.env(parent = emptyenv())
