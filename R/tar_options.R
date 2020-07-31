#' @title Set target options.
#' @export
#' @description Set target options, including default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue. See default options with [tar_option()].
#'   To use `tar_options()` effectively, put it in your workflow's
#'   `_targets.R` script before calls to [tar_target()] or [tar_target_raw()].
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
  tidy_eval = NULL,
  packages = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  deployment = NULL,
  resources = NULL,
  template = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL
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
  tidy_eval <- tidy_eval %||% tar_option("tidy_eval")
  assert_lgl(tidy_eval, "tidy_eval in tar_options() must be logical.")
  assign("tidy_eval", tidy_eval, envir = tar_envir_options)
}

tar_options_packages <- function(packages) {
  packages <- packages %||% tar_option("packages")
  assert_chr(packages, "packages in tar_options() must be character.")
  assign("packages", packages, envir = tar_envir_options)
}

tar_options_library <- function(library) {
  library <- library %||% tar_option("library")
  assert_chr(library %||% character(0), "library must be NULL or character.")
  assign("library", library, envir = tar_envir_options)
}

tar_options_envir <- function(envir) {
  envir <- envir %||% tar_option("envir")
  msg <- paste(
    "envir in tar_options() must be the environment",
    "where you put your functions and global objects",
    "(global environment for most users)."
  )
  assert_envir(envir, msg)
  assign("envir", envir, envir = tar_envir_options)
}

tar_options_format <- function(format) {
  format <- format %||% tar_option("format")
  format <- match.arg(format, store_formats())
  assign("format", format, envir = tar_envir_options)
}

tar_options_iteration <- function(iteration) {
  iteration <- iteration %||% tar_option("iteration")
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  assign("iteration", iteration, envir = tar_envir_options)
}

tar_options_error <- function(error) {
  error <- error %||% tar_option("error")
  error <- match.arg(error, c("stop", "continue"))
  assign("error", error, envir = tar_envir_options)
}

tar_options_memory <- function(memory) {
  memory <- memory %||% tar_option("memory")
  memory <- match.arg(memory, c("persistent", "transient"))
  assign("memory", memory, envir = tar_envir_options)
}

tar_options_deployment <- function(deployment) {
  deployment <- deployment %||% tar_option("deployment")
  deployment <- match.arg(deployment, c("remote", "local"))
  assign("deployment", deployment, envir = tar_envir_options)
}

tar_options_resources <- function(resources) {
  resources <- resources %||% tar_option("resources")
  assert_list(resources, "resources in tar_options() must be a named list.")
  assign("resources", resources, envir = tar_envir_options)
}

tar_options_template <- function(template) {
  template <- template %||% tar_option("template")
  warn_template(template)
  assign("template", template, envir = tar_envir_options)
}

tar_options_storage <- function(storage) {
  storage <- storage %||% tar_option("storage")
  storage <- match.arg(storage, c("local", "remote"))
  assign("storage", storage, envir = tar_envir_options)
}

tar_options_retrieval <- function(retrieval) {
  retrieval <- retrieval %||% tar_option("retrieval")
  retrieval <- match.arg(retrieval, c("local", "remote"))
  assign("retrieval", retrieval, envir = tar_envir_options)
}

tar_options_cue <- function(cue) {
  cue <- cue %||% tar_option("cue")
  trn(is.null(cue), NULL, cue_validate(cue))
  assign("cue", cue, envir = tar_envir_options)
}

tar_options_debug <- function(debug) {
  debug <- debug %||% tar_option("debug")
  assert_chr(debug, "debug artument of tar_options() must be a character.")
  assign("debug", debug, envir = tar_envir_options)
}
