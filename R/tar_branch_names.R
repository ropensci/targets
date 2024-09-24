#' @title Branch names
#' @export
#' @family branching
#' @description Get the branch names of a dynamic branching target
#'   using numeric indexes.
#'   [tar_branch_names()] expects an unevaluated symbol
#'   for the `name` argument, whereas [tar_branch_names_raw()]
#'   expects a character string for `name`.
#' @return A character vector of branch names.
#' @param name Name of the dynamic branching target.
#'   [tar_branch_names()] expects an unevaluated symbol
#'   for the `name` argument, whereas [tar_branch_names_raw()]
#'   expects a character string for `name`.
#' @param index Integer vector of branch indexes.
#' @param store Character string, directory path to the
#'   `targets` data store of the pipeline.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(x, seq_len(4)),
#'     tar_target(y, 2 * x, pattern = map(x)),
#'     tar_target(z, y, pattern = map(y))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_branch_names(z, c(2, 3))
#' })
#' }
tar_branch_names <- function(
  name,
  index,
  store = targets::tar_config_get("store")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_branch_names_raw(name = name, index = index, store = store)
}
