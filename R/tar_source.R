#' @title Run R scripts.
#' @export
#' @family utilities
#' @description Run all the R scripts in a directory
#'   in the environment specified.
#' @details `tar_source()` is a convenient way to load R scripts
#'   in `_targets.R` to make custom functions available to the pipeline.
#'   `tar_source()` recursively looks for files ending
#'   in `.R` or `.r`, and it runs each with
#'   `eval(parse(text = readLines(script_file, warn = FALSE)), envir)`.
#' @inheritSection tar_meta Storage access
#' @return `NULL` (invisibly)
#' @param files Character vector of file and directory paths
#'   to look for R scripts to run. Paths must either be absolute
#'   paths or must be relative to the current working directory
#'   just before the function call.
#' @param envir Environment to run the scripts. Defaults to
#'   `tar_option_get("envir")`, the environment of the pipeline.
#' @param change_directory Logical, whether to temporarily change
#'   the working directory to the directory of each R script
#'   before running it.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' # Running in tar_dir(), these files are written in tempdir().
#' dir.create("R")
#' writeLines("f <- function(x) x + 1", file.path("R", "functions.R"))
#' tar_script({
#'   tar_source()
#'   list(tar_target(x, f(1)))
#' })
#' tar_make()
#' tar_read(x) # 2
#' })
#' }
tar_source <- function(
  files = "R",
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
) {
  tar_assert_lgl(change_directory)
  tar_assert_scalar(change_directory)
  tar_assert_chr(files)
  tar_assert_nzchar(files)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files)) {
    tar_message_validate(
      "tar_source(): these files do not exist: ",
      paste(missing_files, collapse = ", ")
    )
  }
  all_files <- unique(unlist(lapply(files, file_list_files)))
  r_scripts <- grep(pattern = "\\.[rR]$", x = all_files, value = TRUE)
  non_r_scripts <- setdiff(all_files, r_scripts)
  if (length(non_r_scripts)) {
    tar_message_validate(
      "tar_source() only sources R scripts. Ignoring non-R files: ",
      paste(non_r_scripts, collapse = ", ")
    )
  }
  lapply(
    r_scripts,
    function(script) {
      if (change_directory) {
        dir <- dirname(script)
        old <- eval(parse(text = "setwd(dir)"))
        on.exit(eval(parse(text = "setwd(old)")))
        expr <- parse(file = basename(script), keep.source = TRUE)
      } else {
        expr <- parse(file = script, keep.source = TRUE)
      }
      eval(expr = expr, envir = envir)
      invisible()
    }
  )
  invisible()
}
