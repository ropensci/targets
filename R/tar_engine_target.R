#' @title `knitr` engine for targets
#' @export
#' @family Target Markdown
#' @seealso <https://books.ropensci.org/targets/markdown.html>
#' @description `knitr` engine to prototype and construct
#'   targets in a literate programming document.
#' @section Target Markdown:
#'   Target Markdown is an interface to prototype and construct
#'   `targets` pipelines entirely in R Markdown. See
#'   <https://books.ropensci.org/targets/markdown.html>
#'   for usage details.
#' @details [tar_engine_target()] is a `knitr` engine to
#'   prototype and construct different kinds of targets.
#'   These engines have two basic behaviors,
#'   depending on whether the R session is interactive.
#'   Interactive sessions are for prototyping,
#'   while non-interactive sessions are for pipeline construction.
#'   In interactive sessions, the engine runs the code in a new
#'   transient environment (in order prevent side effects)
#'   and then assigns the return value
#'   to a variable whose name is the chunk name. This emulates
#'   what happens when a target runs during [tar_make()].
#'   In non-interactive sessions, the engine adds the code
#'   to the existing pipeline without running it. More specifically,
#'   it generates a call to the target factory and
#'   writes that call to a file in `_targets_r/targets/`.
#'   In addition, the engine also writes a `_targets.R`
#'   file that leverages the files in `_targets_r/`
#'   to manage the pipeline.
#' @param options Named list of `knitr` chunk options.
#' @param package Character of length 1, name of the package
#'   that supports the target factory.
#' @param factory Character of length 1, name of the target factory
#'   corresponding to the engine. Defaults to `"tar_target"`
#'   but could be an alternative factory such as `"tar_render"`
#'   (from `tarchetypes`) or `"tar_stan_mcmc"` (from `stantargets`).
#' @param code Character of length 1, name of a formal argument
#'   to the target factory to insert the code from the chunk.
#' @param interactive Logical of length 1, whether to run in interactive
#'   mode (prototyping) or non-interactive mode (pipeline construction).
#' @param namer A function that accepts a character
#'   argument of length 1 and returns a character of length 1.
#'   In interactive mode, the engine usually assigns
#'   the chunk's return value to a variable whose name is the chunk name.
#'   This practice does not make sense for all target factories.
#'   For example, `stantargets::tar_stan_mcmc(target_name, ...)`
#'   creates many different targets, and the user-supplied R command
#'   generates the upstream Stan data. In this case, the engine
#'   should have `namer = function(name) paste0(name, "_data")`.
#'   That way, when the user interactively runs a
#'   `{tar_stan_mcmc target_name}` code chunk, the
#'   engine will assign the return value to a variable named
#'   `target_name_data`.
tar_engine_target <- function(
  options,
  package = "targets",
  factory = "tar_target",
  code = "command",
  interactive = interactive,
  namer = identity
) {
  assert_list(options, "chunk options must be a list.")
  assert_nonempty(names(options), "chunk options list must be named.")
  assert_nzchar(names(options), "each chunk option name must not be empty.")
  assert_chr(package, "package must be a character.")
  assert_chr(factory, "factory must be a character.")
  assert_chr(code, "code must be a character.")
  assert_lgl(interactive, "interactive must be logical.")
  assert_scalar(package, "package must have length 1.")
  assert_scalar(factory, "factory must have length 1.")
  assert_scalar(code, "code must have length 1.")
  assert_scalar(interactive, "interactive must have length 1.")
  assert_function(namer, "namer must be a function.")
  if_any(
    interactive,
    tar_engine_target_interactive(
      options = options,
      code = code,
      namer = namer
    ),
    tar_engine_target_noninteractive(
      options = options,
      package = package,
      factory = factory,
      code = code
    )
  )
}

tar_engine_target_interactive <- function(
  options,
  code,
  namer
) {
  assert_package("knitr")
  envir <- knitr::knit_global()
  browser()
}

tar_engine_target_noninteractive <- function(
  options,
  package,
  factory,
  code
) {
  write_targets_r()
  browser()
}

write_targets_r <- function() {
  path <- system.file(
    file.path("pipelines", "_targets_r.R"),
    package = "targets",
    mustWork = TRUE
  )
  file.copy(path, path_script(), overwrite = TRUE)
  invisible()
}
