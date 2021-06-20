#' @title Branch names (raw version)
#' @export
#' @family branching
#' @description Get the branch names of a dynamic branching target
#'   using numeric indexes. Same as [tar_branch_names()] except
#'   `name` is a character of length 1.
#' @return A character vector of branch names.
#' @inheritParams tar_meta
#' @param name Character of length 1,
#'   name of the dynamic branching target (pattern).
#' @param index Integer vector of branch indexes.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(w, 1),
#'     tar_target(x, seq_len(4)),
#'     tar_target(y, 2 * x, pattern = map(x)),
#'     tar_target(z, y, pattern = map(y))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_branch_names_raw("z", c(2, 3))
#' })
#' }
tar_branch_names_raw <- function(
  name,
  index,
  store = targets::tar_config_get("store")
) {
  assert_chr(name, "name must be a character.")
  assert_scalar(name, "name must have length 1.")
  assert_dbl(index, "index must be numeric.")
  meta <- meta_init(path_store = store)
  meta <- tibble::as_tibble(meta$database$read_condensed_data())
  assert_in(name, meta$name, paste(name, "not in metadata."))
  children <- meta$children[meta$name == name][[1]]
  if_any(
    anyNA(children),
    throw_validate("target ", name, " has no branches."),
    children[index]
  )
}
