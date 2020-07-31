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
#'   Must be one of the argument names of [tar_options()].
#' @examples
#' tar_option("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_options(format = "fst_tbl") # new default format
#' tar_option("format")
#' tar_target(x, 1)$settings$format
#' tar_options(format = "rds") # reset the format
tar_option <- function(option) {
  option <- match.arg(option, choices = names(formals(tar_options)))
  tar_envir_options[[option]] %||% tar_option_default(option)
}

tar_option_default <- function(option) {
  switch(
    option,
    tidy_eval = TRUE,
    packages = (.packages()),
    library = NULL,
    envir = globalenv(),
    format = "rds",
    iteration = "vector",
    error = "stop",
    memory = "persistent",
    deployment = "remote",
    resources = list(),
    template = NULL,
    storage = "local",
    retrieval = "local",
    cue = targets::tar_cue(),
    debug = character(0)
  )
}
