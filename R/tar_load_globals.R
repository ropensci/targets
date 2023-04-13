#' @title Load globals for debugging, testing, and prototyping
#' @export
#' @family debug
#' @description Load user-defined packages, functions, global objects, and
#'   settings defined in the target script file (default: `_targets.R`).
#'   This function is for debugging, testing, and prototyping only.
#'   It is not recommended for use inside a serious pipeline
#'   or to report the results of a serious pipeline.
#' @details This function first sources the target script file
#'   (default: `_targets.R`)
#'   to loads all user-defined functions, global objects, and settings
#'   into the current R process. Then, it loads all the packages defined
#'   in `tar_option_get("packages")` (default: `(.packages())`)
#'   using `library()` with `lib.loc` defined in `tar_option_get("library")`
#'   (default: `NULL`).
#' @return `NULL` (invisibly).
#' @inheritParams tar_config_set
#' @param envir Environment to source the target script (default: `_targets.R`).
#'   Defaults to the calling environment.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   tar_option_set(packages = "callr")
#'   analyze_data <- function(data) {
#'     summary(data)
#'   }
#'   list(
#'     tar_target(x, 1 + 1),
#'     tar_target(y, 1 + 1)
#'   )
#' }, ask = FALSE)
#' tar_load_globals()
#' print(analyze_data)
#' print("callr" %in% (.packages()))
#' })
#' }
tar_load_globals <- function(
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  force(envir)
  tar_assert_script(script)
  eval(parse(text = readLines(script)), envir = envir)
  map(
    x = tar_option_get("packages"),
    f = library,
    character.only = TRUE,
    lib.loc = tar_option_get("library")
  )
  invisible()
}
