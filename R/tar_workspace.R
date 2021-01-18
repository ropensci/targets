#' @title Load a saved workspace and seed for debugging.
#' @export
#' @description Load the packages, workspace, and random number generator seed
#'   of target attempted with a workspace file.
#' @details If you set `error = "workspace"` in [tar_option_set()]
#'   or [tar_target()], then if that target throws an error
#'   in [tar_make()], it will save its workspace to an RDS file
#'   in `_targets/workspaces/`. Workspaces also get saved for targets
#'   supplied to the `workspace` argument of [tar_option_set()],
#'   even individual branches. The workspace is a compact reference
#'   that allows `tar_workspace()` to load the target's dependencies
#'   and random number generator seed as long as the data objects
#'   are still in the data store (usually files in `_targets/objects/`).
#'   When you are done debugging, you can remove the workspace files
#'   using `tar_destroy(destroy = "workspaces")`.
#' @return This function returns `NULL`, but it does load
#'   the target's required packages, as well as multiple objects
#'   into the environment (`envir` argument) in order to replicate the
#'   workspace where the error happened. These objects include
#'   the global objects at the time [tar_make()] was called and the
#'   dependency targets. The random number generator seed for the
#'   target is also assigned with `set.seed()`
#' @param name Symbol, name of the target whose workspace to read.
#' @param envir Environment in which to put the objects.
#' @param packages Logical, whether to load the required packages
#'   of the target.
#' @param source Logical, whether to run `_targets.R` to load user-defined
#'   global object dependencies into `envir`. If `TRUE`, then `envir`
#'   should either be the global environment or inherit from the
#'   global environment.
#' @examples
#' if (identical(Sys.getenv("TARGETS_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # Write all files to a temporary directory.
#' tmp <- sample(1)
#' tar_script({
#'   tar_option_set(error = "workspace")
#'   list(
#'     tar_target(x, "loaded"),
#'     tar_target(y, stop(x))
#'   )
#' })
#' # The following code throws an error for demonstration purposes.
#' try(tar_make())
#' exists("x") # Should be FALSE.
#' tail(.Random.seed)
#' tar_workspace(y)
#' exists("x") # Should be TRUE.
#' print(x) # "loaded"
#' tail(.Random.seed) # Should be different.
#' })
#' }
tar_workspace <- function(
  name,
  envir = parent.frame(),
  packages = TRUE,
  source = TRUE
) {
  force(envir)
  name <- deparse_language(substitute(name))
  assert_chr(name)
  assert_scalar(name)
  workspace <- workspace_read(name)
  workspace_populate(workspace)
  workspace_assign(workspace, envir)
  if (packages) {
    command <- workspace$target$command
    build_load_packages(command$packages, command$library)
  }
  if (source) {
    source("_targets.R", local = envir)
  }
  set.seed(workspace$target$command$seed)
  invisible()
}
