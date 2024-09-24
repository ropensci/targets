#' @rdname tar_branch_names
#' @export
tar_branch_names_raw <- function(
  name,
  index,
  store = targets::tar_config_get("store")
) {
  tar_assert_chr(name)
  tar_assert_scalar(name)
  tar_assert_dbl(index)
  tar_assert_store(store = store)
  tar_assert_meta(store = store)
  meta <- meta_init(path_store = store)
  meta <- tibble::as_tibble(meta$database$read_condensed_data())
  tar_assert_in(name, meta$name, paste(name, "not in metadata."))
  children <- meta$children[meta$name == name][[1]]
  if_any(
    anyNA(children),
    tar_throw_validate("target ", name, " has no branches."),
    children[index]
  )
}
