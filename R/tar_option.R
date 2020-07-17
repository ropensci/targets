#' @title Get a target option.
#' @export
#' @description Get a target option. These options include default arguments to
#'   [tar_target()] such as packages, storage format,
#'   iteration type, and cue.
#'   Needs to be called before any calls to [tar_target()]
#'   in order to take effect.
#' @details This function goes well with [tar_target_raw()] when it comes
#'   to defining external interfaces on top of the `targets` package to create
#'   pipelines.
#' @return Value of a target option. Returns `NULL` if the option is not set.
#' @param option Character of length 1, name of an option to get.
#'   Must be one of the argument names of [tar_options()].
#' @param default Default value if the option is unset (`NULL`)
#' @examples
#' \dontrun{
#' # Set target options:
#' tar_options(format = "fst_tbl")
#' tar_option("format")
#' # Reset target options:
#' tar_options()
#' tar_option("format")
#' }
tar_option <- function(option, default = NULL) {
  option <- match.arg(option, choices = names(formals(tar_options)))
  envir_target[[option]] %||% default
}
