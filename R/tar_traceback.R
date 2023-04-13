#' @title Get a target's traceback
#' @export
#' @family debug
#' @description Return the saved traceback of a target.
#'   Assumes the target errored out in a previous run of the pipeline
#'   with workspaces enabled for that target.
#'   See [tar_workspace()] for details.
#' @return Character vector, the traceback of a failed target
#'   if it exists.
#' @inheritParams tar_validate
#' @param name Symbol, name of the target whose workspace to read.
#' @param characters Positive integer. Each line of the traceback
#'   is shortened to this number of characters.
#' @param envir Deprecated in `targets` > 0.3.1 (2021-03-28).
#' @param packages Logical, whether to load the required packages
#'   of the target.
#' @param source Logical, whether to run the target script file
#'   (default: `_targets.R`) to load user-defined
#'   global object dependencies into `envir`. If `TRUE`, then `envir`
#'   should either be the global environment or inherit from the
#'   global environment.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tmp <- sample(1)
#' tar_script({
#'   tar_option_set(workspace_on_error = TRUE)
#'   list(
#'     tar_target(x, "loaded"),
#'     tar_target(y, stop(x))
#'   )
#' }, ask = FALSE)
#' try(tar_make())
#' tar_traceback(y, characters = 60)
#' })
#' }
tar_traceback <- function(
  name,
  envir = NULL,
  packages = NULL,
  source = NULL,
  characters = getOption("width"),
  store = targets::tar_config_get("store")
) {
  tar_assert_scalar(characters, "characters must have length 1.")
  tar_assert_dbl(characters, "characters must be numeric.")
  tar_assert_positive(characters, "characters must be positive.")
  if (!is.null(envir) || !is.null(packages) || !is.null(source)) {
    tar_warn_deprecate(
      "The envir, packages, and source arguments of tar_traceback() ",
      "are deprectaed in targets > 0.3.1 (2021-03-28)."
    )
  }
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_scalar(name)
  workspace <- workspace_read(name = name, path_store = store)
  out <- workspace$target$metrics$traceback
  if (is.null(out)) {
    return(character(0))
  }
  min <- max(which(grepl("^build_eval_fce17be7", out))) %||% 1 %||NA% 1
  if (is.finite(min) && length(min) == 1L) {
    out <- out[seq(min + 1, length(out))]
  }
  characters <- min(characters, max(nchar(out)))
  substr(out, 0, characters)
}
