#' @title Set target options.
#' @export
#' @description Set target options, including default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue. Put a call to `tar_options()`
#'   in your workflow's `_targets.R` script for the options to take effect.
#'   Needs to be called before any calls to [tar_target()]
#'   in order to be useful
#' @return Nothing.
#' @inheritParams tar_target
#' @param envir Environment containing functions and global objects
#'   used in the R commands to run targets.
#' @param debug Character vector of names of targets to run in debug mode.
#'   To use effectively, you must set `callr_function = NULL` and
#'   restart your R session just before running.
#'   [tar_make()], [tar_make_clustermq()], or [tar_make_future()].
#'   For any target mentioned in `debug`, `targets` will force the target to
#'   build locally (with `tar_cue(mode = "always")` and `deployment = "local"`
#'   in the settings) and pause in an interactive debugger to help you diagnose
#'   problems. This is like inserting a `browser()` statement at the
#'   beginning of the target's expression, but without invalidating any
#'   targets.
#' @examples
#' tar_option("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_options(format = "fst_tbl") # new default format
#' tar_option("format")
#' tar_target(x, 1)$settings$format
#' tar_options(format = "rds") # reset the format
tar_options <- function(
  tidy_eval = TRUE,
  packages = (.packages()),
  library = NULL,
  envir = parent.frame(),
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
) {
  force(envir)
  tar_options_tidy_eval(tidy_eval)
  tar_options_packages(packages)
  tar_options_library(library)
  tar_options_envir(envir)
  tar_options_format(format)
  tar_options_iteration(iteration)
  tar_options_error(error)
  tar_options_memory(memory)
  tar_options_deployment(deployment)
  tar_options_resources(resources)
  tar_options_template(template)
  tar_options_storage(storage)
  tar_options_retrieval(retrieval)
  tar_options_cue(cue)
  tar_options_debug(debug)
}

tar_options_tidy_eval <- function(tidy_eval) {
  assert_lgl(tidy_eval, "tidy_eval in tar_options() must be logical.")
  assign("tidy_eval", tidy_eval, envir = envir_options)
}

tar_options_packages <- function(packages) {
  assert_chr(packages, "packages in tar_options() must be character.")
  assign("packages", packages, envir = envir_options)
}

tar_options_library <- function(library) {
  assert_chr(library %||% character(0), "library must be NULL or character.")
  assign("library", library, envir = envir_options)
}

tar_options_envir <- function(envir) {
  msg <- paste(
    "envir in tar_options() must be the environment",
    "where you put your functions and global objects",
    "(global environment for most users)."
  )
  assert_envir(envir, msg)
  assign("envir", envir, envir = envir_options)
}

tar_options_format <- function(format) {
  format <- match.arg(format, store_formats())
  assign("format", format, envir = envir_options)
}

tar_options_iteration <- function(iteration) {
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  assign("iteration", iteration, envir = envir_options)
}

tar_options_error <- function(error) {
  error <- match.arg(error, c("stop", "continue"))
  assign("error", error, envir = envir_options)
}

tar_options_memory <- function(memory) {
  memory <- match.arg(memory, c("persistent", "transient"))
  assign("memory", memory, envir = envir_options)
}

tar_options_deployment <- function(deployment) {
  deployment <- match.arg(deployment, c("remote", "local"))
  assign("deployment", deployment, envir = envir_options)
}

tar_options_resources <- function(resources) {
  assert_list(resources, "resources in tar_options() must be a named list.")
  assign("resources", resources, envir = envir_options)
}

tar_options_template <- function(template) {
  warn_template(template)
  assign("template", template, envir = envir_options)
}

tar_options_storage <- function(storage) {
  storage <- match.arg(storage, c("local", "remote"))
  assign("storage", storage, envir = envir_options)
}

tar_options_retrieval <- function(retrieval) {
  retrieval <- match.arg(retrieval, c("local", "remote"))
  assign("retrieval", retrieval, envir = envir_options)
}

tar_options_cue <- function(cue) {
  trn(is.null(cue), NULL, cue_validate(cue))
  assign("cue", cue, envir = envir_options)
}

tar_options_debug <- function(debug) {
  assert_chr(debug, "debug artument of tar_options() must be a character.")
  assign("debug", debug, envir = envir_options)
}
