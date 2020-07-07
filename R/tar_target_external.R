#' @title Declare a target from an external interface.
#' @export
#' @description `tar_target_external()` is intended
#'   for people who develop infrastructure on top of `targets`,
#'   and it is not relevant to most end users.
#' @details `tar_target_external()` is like [tar_target()],
#'   but it is designed to be called from external interfaces.
#'   Argument `name` is a character instead of a symbol, and
#'   arguments `expr` and `pattern` are expression objects
#'   instead of quoted code. In other words, where [tar_target()] uses
#'   non-standard evaluation, `tar_target_external()` relies totally
#'   on standard evaluation.
#'   In addition, `tar_target_external()` supports arguments `string`
#'   and `deps` to take more control of how commands and their dependencies
#'   are resolved.
#'   The hope is to make it easier for other packages to metaprogram
#'   their own `targets` pipelines and develop external
#'   domain specific languages for static branching.
#' @return A target object. Users should not modify these directly,
#'   just feed them to [tar_pipeline()] in your `_targets.R` file.
#' @inheritParams tar_target
#' @inheritParams tar_options
#' @param name Character of length 1, name of the target.
#' @param expr Similar to the `expr` argument of [`tar_target()`] except
#'   the object must already be an expression instead of
#'   informally quoted code.
#'   `base::expression()` and `base::quote()` can produce such objects.
#' @param pattern Similar to the `pattern` argument of [`tar_target()`]
#'   except the object must already be an expression instead of
#'   informally quoted code.
#'   `base::expression()` and `base::quote()` can produce such objects.
#' @param deps Optional character vector of the adjacent upstream
#'   dependencies of the target, including targets and global objects.
#'   If `NULL`, dependencies are resolved automatically as usual.
#' @param string Optional string representation of the command.
#'   Internally, the string gets hashed to check if the command changed
#'   since last run, which helps `targets` decide whether the
#'   target is up to date. External interfaces can take control of
#'   `string` to ignore changes in certain parts of the command.
#'   If `NULL`, the strings is just deparsed from `expr` (default).
#' @examples
#'   # The following are equivalent.
#'   y <- tar_target(y, sqrt(x), pattern = map(x))
#'   y <- tar_target_external("y", expression(sqrt(x)), expression(map(x)))
tar_target_external <- function(
  name,
  expr,
  pattern = NULL,
  packages = targets::tar_option("packages", (.packages())),
  library = targets::tar_option("library"),
  deps = NULL,
  string = NULL,
  envir = tar_option("envir", globalenv()),
  format = targets::tar_option("format", "rds"),
  iteration = targets::tar_option("iteration", "vector"),
  error = targets::tar_option("error", "stop"),
  memory = targets::tar_option("memory", "persistent"),
  deployment = targets::tar_option("deployment", "remote"),
  template = targets::tar_option("template", NULL),
  resources = targets::tar_option("resources", list()),
  storage = targets::tar_option("storage", "local"),
  retrieval = targets::tar_option("retrieval", storage),
  cue = targets::tar_option("cue", NULL)
) {
  force(envir)
  assert_chr(name, "name arg of tar_target_external() must be character")
  assert_chr(packages, "packages in tar_target_external() must be character.")
  assert_chr(
    library %||% character(0),
    "library in tar_target_external() must be NULL or character."
  )
  format <- match.arg(format, store_formats())
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  error <- match.arg(error, c("stop", "continue"))
  memory <- match.arg(memory, c("persistent", "transient"))
  deployment <- match.arg(deployment, c("remote", "local"))
  warn_template(template)
  assert_list(
    resources,
    "resources in tar_target_external() must be a named list."
  )
  storage <- match.arg(storage, c("local", "remote"))
  retrieval <- match.arg(retrieval, c("local", "remote"))
  if (!is.null(cue)) {
    cue_validate(cue)
  }
  target_init(
    name = name,
    expr = expr,
    pattern = pattern,
    packages = packages,
    library = library,
    envir = envir,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
