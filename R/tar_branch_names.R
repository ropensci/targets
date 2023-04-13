#' @title Branch names
#' @export
#' @family branching
#' @description Get the branch names of a dynamic branching target
#'   using numeric indexes.
#' @return A character vector of branch names.
#' @inheritParams tar_branch_names_raw
#' @param name Symbol, name of the dynamic branching target (pattern).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
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
