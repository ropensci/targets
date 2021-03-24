#' @title Get a target option.
#' @export
#' @description Get a target option. These options include default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue.
#'   Needs to be called before any calls to [tar_target()]
#'   in order to take effect.
#' @details This function goes well with [tar_target_raw()] when it comes
#'   to defining external interfaces on top of the `targets` package to create
#'   pipelines.
#' @return Value of a target option.
#' @param option Character of length 1, name of an option to get.
#'   Must be one of the argument names of [tar_option_set()].
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
tar_option_get <- function(option) {
  assert_flag(option, choices = names(formals(tar_option_set)))
  tar_envir_options[[option]] %|||% tar_option_default(option)
}

tar_option_default <- function(option) {
  switch(
    option,
    tidy_eval = TRUE,
    packages = (.packages()),
    imports = character(0),
    library = NULL,
    envir = globalenv(),
    format = "rds",
    iteration = "vector",
    error = "stop",
    memory = "persistent",
    garbage_collection = FALSE,
    deployment = "worker",
    priority = 0,
    backoff = 5,
    resources = list(),
    storage = "main",
    retrieval = "main",
    cue = targets::tar_cue(),
    debug = character(0),
    workspaces = character(0)
  )
}
