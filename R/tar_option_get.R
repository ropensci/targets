#' @title Get a target option.
#' @export
#' @family configuration
#' @description Get a target option. These options include default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue.
#'   Needs to be called before any calls to [tar_target()]
#'   in order to take effect.
#' @details This function goes well with [tar_target_raw()] when it comes
#'   to defining external interfaces on top of the `targets` package to create
#'   pipelines.
#' @return Value of a target option.
#' @param name Character of length 1, name of an option to get.
#'   Must be one of the argument names of [tar_option_set()].
#' @param option Deprecated, use the `name` argument instead.
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
tar_option_get <- function(name = NULL, option = NULL) {
  if (!is.null(option)) {
    tar_warn_deprecate(
      "the option argument of tar_option_get() ",
      "was deprecated in targets version 0.5.0.9000 (2021-05-30). ",
      "use the name argument instead."
    )
    name <- option
  }
  tar_assert_nonempty(name)
  tar_assert_flag(name, choices = names(formals(tar_option_set)))
  switch(
    name,
    tidy_eval = tar_options$get_tidy_eval(),
    packages = tar_options$get_packages(),
    imports = tar_options$get_imports(),
    library = tar_options$get_library(),
    envir = tar_options$get_envir(),
    format = tar_options$get_format(),
    repository = tar_options$get_repository(),
    iteration = tar_options$get_iteration(),
    error = tar_options$get_error(),
    memory = tar_options$get_memory(),
    garbage_collection = tar_options$get_garbage_collection(),
    deployment = tar_options$get_deployment(),
    priority = tar_options$get_priority(),
    backoff = tar_options$get_backoff(),
    resources = tar_options$get_resources(),
    storage = tar_options$get_storage(),
    retrieval = tar_options$get_retrieval(),
    cue = tar_options$get_cue(),
    debug = tar_options$get_debug(),
    workspaces = tar_options$get_workspaces(),
    workspace_on_error = tar_options$get_workspace_on_error(),
    seed = tar_options$get_seed(),
    controller = tar_options$get_controller()
  )
}
