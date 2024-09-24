#' @title Integer branch indexes
#' @export
#' @family branching
#' @description Get the integer indexes of individual branch names
#'   within their corresponding dynamic branching targets.
#' @return A named integer vector of branch indexes.
#' @inheritParams tar_meta
#' @param names Character vector of branch names.
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
#' names <- c(
#'   tar_meta(y, children)$children[[1]][c(2, 3)],
#'   tar_meta(z, children)$children[[1]][2]
#' )
#' names
#' tar_branch_index(names) # c(2, 3, 2)
#' })
#' }
tar_branch_index <- function(names, store = targets::tar_config_get("store")) {
  tar_assert_chr(names)
  tar_assert_store(store = store)
  tar_assert_meta(store = store)
  meta <- meta_init(path_store = store)
  meta <- tibble::as_tibble(meta$database$read_condensed_data())
  missing_branches <- setdiff(names, meta$name[meta$type == "branch"])
  if (length(missing_branches)) {
    tar_throw_validate(
      "missing branches in metadata: ",
      paste(missing_branches, collapse = ", ")
    )
  }
  parents <- meta$parent[meta$type == "branch"]
  missing_patterns <- setdiff(parents, meta$name)
  if (length(missing_patterns)) {
    tar_throw_validate(
      "missing dynamic targets in metadata: ",
      paste(missing_patterns, collapse = ", ")
    )
  }
  map_int(names, ~tar_branch_index_branch(.x, meta))
}

tar_branch_index_branch <- function(name, meta) {
  parent <- meta$parent[meta$name == name]
  children <- meta$children[meta$name == parent][[1]]
  if_any(
    name %in% children,
    match(name, children),
    tar_throw_validate(
      "branch ",
      name,
      " is not part of dynamic target ",
      parent
    )
  )
}
