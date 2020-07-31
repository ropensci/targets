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
  assert_lgl(tidy_eval, "tidy_eval in tar_options() must be logical.")
  assert_chr(packages, "packages in tar_options() must be character.")
  assert_chr(library %||% character(0), "library must be NULL or character.")
  assert_envir(envir, tar_options_envir_message())
  format <- match.arg(format, store_formats())
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  error <- match.arg(error, c("stop", "continue"))
  memory <- match.arg(memory, c("persistent", "transient"))
  deployment <- match.arg(deployment, c("remote", "local"))
  warn_template(template)
  assert_list(resources, "resources in tar_options() must be a named list.")
  storage <- match.arg(storage, c("local", "remote"))
  retrieval <- match.arg(retrieval, c("local", "remote"))
  trn(is.null(cue), NULL, cue_validate(cue))
  assert_chr("debug", "debug artument of tar_options() must be a character.")
  assign("tidy_eval", tidy_eval, envir = envir_target)
  assign("packages", packages, envir = envir_target)
  assign("library", library, envir = envir_target)
  assign("envir", envir, envir = envir_target)
  assign("format", format, envir = envir_target)
  assign("iteration", iteration, envir = envir_target)
  assign("error", error, envir = envir_target)
  assign("memory", memory, envir = envir_target)
  assign("deployment", deployment, envir = envir_target)
  assign("resources", resources, envir = envir_target)
  assign("template", template, envir = envir_target)
  assign("storage", storage, envir = envir_target)
  assign("retrieval", retrieval, envir = envir_target)
  assign("cue", cue, envir = envir_target)
  assign("debug", debug, envir = envir_target)
}

tar_options_envir_message <- function() {
  paste(
    "envir in tar_options() must be the environment",
    "where you put your functions and global objects",
    "(global environment for most users)."
  )
}
