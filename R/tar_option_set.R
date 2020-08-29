#' @title Set target options.
#' @export
#' @description Set target options, including default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue. See default options with [tar_option_get()].
#'   To use `tar_option_set()` effectively, put it in your workflow's
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
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_reset() # reset the format
#' tar_target(x, 1)$settings$format
tar_option_set <- function(
  tidy_eval = NULL,
  packages = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  deployment = NULL,
  priority = NULL,
  resources = NULL,
  template = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL
) {
  force(envir)
  tar_option_set_tidy_eval(tidy_eval)
  tar_option_set_packages(packages)
  tar_option_set_library(library)
  tar_option_set_envir(envir)
  tar_option_set_format(format)
  tar_option_set_iteration(iteration)
  tar_option_set_error(error)
  tar_option_set_memory(memory)
  tar_option_set_deployment(deployment)
  tar_option_set_priority(priority)
  tar_option_set_resources(resources)
  tar_option_set_template(template)
  tar_option_set_storage(storage)
  tar_option_set_retrieval(retrieval)
  tar_option_set_cue(cue)
  tar_option_set_debug(debug)
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
  format <- match.arg(format, store_formats())
  assign("format", format, envir = tar_envir_options)
}

tar_option_set_iteration <- function(iteration) {
  iteration <- iteration %||% tar_option_get("iteration")
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  assign("iteration", iteration, envir = tar_envir_options)
}

tar_option_set_error <- function(error) {
  error <- error %||% tar_option_get("error")
  error <- match.arg(error, c("stop", "continue"))
  assign("error", error, envir = tar_envir_options)
}

tar_option_set_memory <- function(memory) {
  memory <- memory %||% tar_option_get("memory")
  memory <- match.arg(memory, c("persistent", "transient"))
  assign("memory", memory, envir = tar_envir_options)
}

tar_option_set_deployment <- function(deployment) {
  deployment <- deployment %||% tar_option_get("deployment")
  deployment <- match.arg(deployment, c("remote", "local"))
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

tar_option_set_template <- function(template) {
  template <- template %||% tar_option_get("template")
  warn_template(template)
  assign("template", template, envir = tar_envir_options)
}

tar_option_set_storage <- function(storage) {
  storage <- storage %||% tar_option_get("storage")
  storage <- match.arg(storage, c("local", "remote"))
  assign("storage", storage, envir = tar_envir_options)
}

tar_option_set_retrieval <- function(retrieval) {
  retrieval <- retrieval %||% tar_option_get("retrieval")
  retrieval <- match.arg(retrieval, c("local", "remote"))
  assign("retrieval", retrieval, envir = tar_envir_options)
}

tar_option_set_cue <- function(cue) {
  cue <- cue %||% tar_option_get("cue")
  trn(is.null(cue), NULL, cue_validate(cue))
  assign("cue", cue, envir = tar_envir_options)
}

tar_option_set_debug <- function(debug) {
  debug <- debug %||% tar_option_get("debug")
  assert_chr(debug, "debug artument of tar_option_set() must be a character.")
  assign("debug", debug, envir = tar_envir_options)
}
