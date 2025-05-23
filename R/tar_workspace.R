#' @title Load a locally saved workspace and seed for debugging.
#' @export
#' @family debug
#' @description Load the packages, environment, and random number generator
#'   seed of a target.
#' @details If you activate workspaces through the `workspaces` argument
#'   of [tar_option_set()], then under the circumstances you specify,
#'   `targets` will save a special workspace file to a location in
#'   in `_targets/workspaces/`. The workspace file is a compact reference
#'   that allows `tar_workspace()` to load the target's dependencies
#'   and random number generator seed as long as the data objects
#'   are still in the data store (usually files in `_targets/objects/`).
#'   When you are done debugging, you can remove the workspace files
#'   using `tar_destroy(destroy = "workspaces")`.
#'
#'   If `tar_option_get("repository_meta")` is `"aws"` or `"gcp"`, then
#'   [tar_make()] uploads workspaces to the bucket and prefix provided.
#'   Download one of these workspaces with [tar_workspace_download()].
#'   Downloaded workspaces can be loaded the usual way with
#'   [tar_workspace()], and you should see them in
#'   character vector returned by [tar_workspaces()].
#' @return This function returns `NULL`, but it does load
#'   the target's required packages, as well as multiple objects
#'   into the environment (`envir` argument) in order to replicate the
#'   workspace where the error happened. These objects include
#'   the global objects at the time [tar_make()] was called and the
#'   dependency targets. The random number generator seed for the
#'   target is also assigned with [tar_seed_set()].
#' @inheritParams tar_validate
#' @param name Symbol, name of the target whose workspace to read.
#' @param envir Environment in which to put the objects.
#' @param packages Logical, whether to load the required packages
#'   of the target.
#' @param source Logical, whether to run `_targets.R` to load user-defined
#'   global object dependencies into `envir`. If `TRUE`, then `envir`
#'   should either be the global environment or inherit from the
#'   global environment.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tmp <- sample(1)
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(x, "loaded"),
#'     tar_target(y, stop(x))
#'   )
#' }, ask = FALSE)
#' # The following code throws an error for demonstration purposes.
#' try(tar_make())
#' exists("x") # Should be FALSE.
#' tail(.Random.seed) # for comparison to the RNG state after tar_workspace(y)
#' tar_workspace(y)
#' exists("x") # Should be TRUE.
#' print(x) # "loaded"
#' # Should be different: tar_workspace() runs
#' # tar_seed_set(tar_meta(y, seed)$seed)
#' tail(.Random.seed)
#' })
#' }
tar_workspace <- function(
  name,
  envir = parent.frame(),
  packages = TRUE,
  source = TRUE,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_workspace", store)
  force(envir)
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_scalar(name)
  workspace <- workspace_read(name = name, path_store = store)
  workspace_populate(workspace)
  workspace_assign(workspace, envir)
  if (packages) {
    workspace_load_packages(workspace)
  }
  if (source) {
    eval(parse(text = readLines(script), keep.source = TRUE), envir = envir)
  }
  workspace_set_seed(workspace)
  invisible()
}
