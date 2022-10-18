#' @title Declare the rules that cue a target.
#' @export
#' @family targets
#' @description Declare the rules that mark a target as outdated.
#' @section Target invalidation rules:
#'   `targets` uses internal metadata and special cues
#'   to decide whether a target is up to date (can skip)
#'   or is outdated/invalidated (needs to rerun). By default,
#'   `targets` moves through the following list of cues
#'   and declares a target outdated if at least one is cue activated.
#'
#'    1. There is no metadata record of the target.
#'    1. The target errored last run.
#'    1. The target has a different class than it did before.
#'    1. The cue mode equals `"always"`.
#'    1. The cue mode does not equal `"never"`.
#'    1. The `command` metadata field (the hash of the R command)
#'     is different from last time.
#'    1. The `depend` metadata field (the hash of the immediate upstream
#'     dependency targets and global objects) is different from last time.
#'    1. The storage format is different from last time.
#'    1. The iteration mode is different from last time.
#'    1. A target's file (either the one in `_targets/objects/`
#'       or a dynamic file) does not exist or changed since last time.
#'
#'   The user can suppress many of the above cues using the `tar_cue()`
#'   function, which creates the `cue` argument of [tar_target()].
#'   Cues objects also constitute more nuanced target invalidation rules.
#'   The `tarchetypes` package has many such examples, including
#'   `tar_age()`, `tar_download()`, `tar_cue_age()`, `tar_cue_force()`,
#'   and `tar_cue_skip()`.
#'
#' @section Dependency-based invalidation and user-defined functions:
#'   If the cue of a target has `depend = TRUE` (default) then the target
#'   is marked invalidated/outdated when its upstream dependencies change.
#'   A target's dependencies include upstream targets,
#'   user-defined functions, and other global objects populated
#'   in the target script file (default: `_targets.R`).
#'   To determine if a given dependency changed
#'   since the last run of the pipeline, `targets` computes hashes.
#'   The hash of a target is computed on its files in storage
#'   (usually a file in `_targets/objects/`). The hash of a
#'   non-function global object dependency is computed directly on its
#'   in-memory data. User-defined functions are hashed in the following way:
#'
#'    1. Deparse the function with `targets:::tar_deparse_safe()`. This
#'      function computes a string representation of the function
#'      body and arguments. This string representation is invariant to
#'      changes in comments and whitespace, which means
#'      trivial changes to formatting do not cue targets to rerun.
#'    1. Manually remove any literal pointers from the function string
#'      using `targets:::mask_pointers()`. Such pointers arise from
#'      inline compiled C/C++ functions.
#'    1. Using static code analysis (i.e. [tar_deps()], which is based on
#'      `codetools::findGlobals()`) identify any user-defined functions
#'      and global objects that the current function depends on.
#'      Append the hashes of those dependencies to the string representation
#'      of the current function.
#'    1. Compute the hash of the final string representation using
#'      `targets:::digest_chr64()`.
#'
#'   Above, (3) is important because user-defined functions
#'   have dependencies of their own, such as other user-defined
#'   functions and other global objects. (3) ensures that a change to
#'   a function's dependencies invalidates the function itself, which
#'   in turn invalidates any calling functions and any targets downstream
#'   with the `depend` cue turned on.
#' @param mode Cue mode. If `"thorough"`, all the cues apply unless
#'   individually suppressed. If `"always"`, then the target always
#'   runs. If `"never"`, then the target does not run unless the
#'   metadata does not exist or the last run errored.
#' @param command Logical, whether to rerun the target if command changed
#'   since last time.
#' @param depend Logical, whether to rerun the target if the value of one
#'   of the dependencies changed.
#' @param format Logical, whether to rerun the target if the user-specified
#'   storage format changed. The storage format is user-specified through
#'   [tar_target()] or [tar_option_set()].
#' @param repository Logical, whether to rerun the target if the user-specified
#'   storage repository changed. The storage repository is user-specified
#'   through [tar_target()] or [tar_option_set()].
#' @param iteration Logical, whether to rerun the target if the user-specified
#'   iteration method changed. The iteration method is user-specified through
#'   [tar_target()] or [tar_option_set()].
#' @param file Logical, whether to rerun the target if the file(s) with the
#'   return value changed or at least one is missing.
#' @param seed Logical, whether to rerun the target if pseudo-random
#'   number generator seed either changed or is `NA`.
#'   The reproducible deterministic target-specific
#'   seeds are controlled by `tar_option_get("seed")` and the target names.
#'   See [tar_option_set()] for details.
#' @examples
#' # The following target will always run when the pipeline runs.
#' x <- tar_target(x, download_data(), cue = tar_cue(mode = "always"))
tar_cue <- function(
  mode = c("thorough", "always", "never"),
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  repository = TRUE,
  iteration = TRUE,
  file = TRUE,
  seed = TRUE
) {
  tar_assert_lgl(command)
  tar_assert_lgl(depend)
  tar_assert_lgl(format)
  tar_assert_lgl(repository)
  tar_assert_lgl(iteration)
  tar_assert_lgl(file)
  tar_assert_lgl(seed)
  tar_assert_scalar(command)
  tar_assert_scalar(depend)
  tar_assert_scalar(format)
  tar_assert_scalar(repository)
  tar_assert_scalar(iteration)
  tar_assert_scalar(file)
  tar_assert_scalar(seed)
  cue_init(
    mode = match.arg(mode),
    command = command,
    depend = depend,
    format = format,
    repository = repository,
    iteration = iteration,
    file = file,
    seed = seed
  )
}
