#' @title Write a target script file.
#' @export
#' @family scripts
#' @description The `tar_script()` function is a convenient
#'   way to create the required target script file (default: `_targets.R`)
#'   in the current working directory.
#'   It always overwrites the existing target script,
#'   and it requires you to be in the working directory
#'   where you intend to write the file, so be careful.
#'   See the "Target script" section for details.
#' @section Target script file:
#'   Every `targets` project requires a target script file.
#'   The target script file is usually a file called `_targets.R`
#'   Functions [tar_make()] and friends look for the target script
#'   and run it to set up the pipeline just prior to the main task.
#'   Every target script file should run the following
#'   steps in the order below:
#'
#'   1. Package: load the `targets` package. This step is automatically
#'       inserted at the top of the target script file produced by
#'       `tar_script()` if `library_targets` is `TRUE`,
#'       so you do not need to explicitly include it in `code`.
#'   2. Globals: load custom functions and global objects into memory.
#'       Usually, this section is a bunch of calls to `source()` that run
#'       scripts defining user-defined functions. These functions support
#'       the R commands of the targets.
#'   3. Options: call [tar_option_set()] to set defaults for targets-specific
#'       settings such as the names of required packages. Even if you have no
#'       specific options to set, it is still recommended to call
#'       [tar_option_set()] in order to register the proper environment.
#'   4. Targets: define one or more target objects using [tar_target()].
#'   5. Pipeline: call [list()] to bring the targets from (3)
#'       together in a pipeline object. Every target script file must return
#'       a pipeline object, which usually means ending with a call to
#'       [list()]. In practice, (3) and (4) can be combined together
#'       in the same function call.
#'
#' @return `NULL` (invisibly).
#' @param code R code to write to the target script file.
#'   If `NULL`, an example target script file is written instead.
#' @param library_targets logical, whether to write a `library(targets)`
#'   line at the top of the target script file automatically (recommended).
#'   If `TRUE`, you do not need to explicitly put `library(targets)`
#'   in `code`.
#' @param ask Logical, whether to ask before writing if the
#'   target script file
#'   already exists. If `NULL`, defaults to `Sys.getenv("TAR_ASK")`.
#'   (Set to `"true"` or `"false"` with `Sys.setenv()`).
#'   If `ask` and the `TAR_ASK` environment variable are both
#'   indeterminate, defaults to `interactive()`.
#' @param script Character of length 1, where to write
#'   the target script file. Defaults to `tar_config_get("script")`,
#'   which in turn defaults to `_targets.R`.
#' @examples
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script() # Writes an example target script file.
#' # Writes a user-defined target script:
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   x <- tar_target(x, 1 + 1)
#'   tar_option_set()
#'   list(x)
#' }, ask = FALSE)
#' writeLines(readLines("_targets.R"))
#' })
tar_script <- function(
  code = NULL,
  library_targets = TRUE,
  ask = NULL,
  script = targets::tar_config_get("script")
) {
  if (!tar_should_overwrite(path = script, ask = ask)) {
    # covered in tests/interactive/test-tar_script.R # nolint
    return(invisible()) # nocov
  }
  tar_assert_lgl(library_targets, "library_targets must be logical.")
  tar_assert_scalar(library_targets, "library_targets must have length 1.")
  code <- substitute(code)
  text <- if_any(
    length(code),
    deparse_script_code(code),
    example_target_script()
  )
  tar_assert_chr(text, "code argument must be parseable R code.")
  if (library_targets) {
    text <- c("library(targets)", text)
  }
  dir_create(dirname(script))
  writeLines(text, script)
  invisible()
}

example_target_script <- function() {
  path <- system.file(
    file.path("pipelines", "example.R"),
    package = "targets",
    mustWork = TRUE
  )
  readLines(path)
}
