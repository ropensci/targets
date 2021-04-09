#' @title Declare the rules that cue a target.
#' @export
#' @family targets
#' @description Declare the rules that mark a target as outdated.
#' @details `targets` uses internal metadata and special cues
#'   to decide if a target is up to date.
#'   A target is outdated if one of the following cues is met
#'   (checked in the order given below). `tar_cue()` can activate
#'   or suppress many of these cues. See the user manual for details.
#'   1. There is no metadata record of the target.
#'   1. The target errored last run.
#'   1. The target has a different class than it did before.
#'   1. The cue mode equals `"always"`.
#'   1. The cue mode does not equal `"never"`.
#'   1. The `command` metadata field (the hash of the R command)
#'     is different from last time.
#'   1. The `depend` metadata field (the hash of the immediate upstream
#'     dependency targets and global objects) is different from last time.
#'   1. The storage format is different from last time.
#'   1. The iteration mode is different from last time.
#'   1. A target's file (either the one in `_targets/objects/`
#'     or a dynamic file) does not exist or changed since last time.
#'
#'    A target's dependencies can include functions, and these functions are
#'  tracked for changes using a custom hashing procedure. When a function's
#'  hash changes, the function is considered invalidated, and so are any
#'  downstream targets with the `depend` cue turned on. The
#'  `targets` package computes the hash of a function in the following way.
#'    1. Deparse the function with `targets:::deparse_safe()`. This
#'      function computes a string representation of the function
#'      that removes comments and standardizes whitespace so that
#'      trivial changes to formatting do not cue targets to rerun.
#'    1. Manually remove any literal pointers from the function string
#'      using `targets:::mask_pointers()`. Such pointers arise from
#'      inline compiled C/C++ functions.
#'    1. Compute a hash on the preprocessed string above using
#'      `targets:::digest_chr64()`.
#'
#'   Those functions themselves have dependencies, and those dependencies
#'   are detected with `codetools::findGlobals()`.
#'   Dependencies of functions may include other global functions or
#'   global objects. If a dependency of a function is invalidated,
#'   the function itself is invalidated, and so are any dependent
#'   targets with the `depend` cue turned on.
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
#' @param iteration Logical, whether to rerun the target if the user-specified
#'   iteration method changed. The iteration method is user-specified through
#'   [tar_target()] or [tar_option_set()].
#' @param file Logical, whether to rerun the target if the file(s) with the
#'   return value changed or at least one is missing.
#' @examples
#' # The following target will always run when the pipeline runs.
#' x <- tar_target(x, download_data(), cue = tar_cue(mode = "always"))
tar_cue <- function(
  mode = c("thorough", "always", "never"),
  command = TRUE,
  depend = TRUE,
  format = TRUE,
  iteration = TRUE,
  file = TRUE
) {
  assert_lgl(command, "command arg of tar_cue() must be logical")
  assert_lgl(depend, "depend arg of tar_cue() must be logical")
  assert_lgl(format, "format arg of tar_cue() must be logical")
  assert_lgl(iteration, "format arg of tar_cue() must be logical")
  assert_lgl(file, "file arg of tar_cue() must be logical")
  assert_scalar(command, "command arg of tar_cue() must be length 1")
  assert_scalar(depend, "depend arg of tar_cue() must be length 1")
  assert_scalar(format, "format arg of tar_cue() must be length 1")
  assert_scalar(iteration, "format arg of tar_cue() must be length 1")
  assert_scalar(file, "file arg of tar_cue() must be length 1")
  cue_init(
    mode = match.arg(mode),
    command = command,
    depend = depend,
    format = format,
    iteration = iteration,
    file = file
  )
}
