#' @rdname tar_read
#' @export
tar_read_raw <- function(
  name,
  branches = NULL,
  meta = tar_meta(store = store),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_read_raw", store)
  tar_assert_store(store = store)
  force(meta)
  tar_assert_chr(name)
  tar_read_inner(name, branches, meta, path_store = store)
}

tar_read_inner <- function(name, branches, meta, path_store) {
  old_store <- tar_runtime$store
  tar_runtime$store <- path_store
  on.exit(tar_runtime$store <- old_store)
  index <- meta$name == name
  if (!any(index)) {
    tar_throw_validate("target ", name, " not found")
  }
  row <- meta[max(which(index)),, drop = FALSE] # nolint
  record <- record_from_row(row = row, path_store = path_store)
  if_any(
    record$type %in% c("stem", "branch"),
    read_builder(record),
    read_pattern(name, record, meta, branches, path_store)
  )
}

read_builder <- function(record) {
  store <- record_bootstrap_store(record)
  file <- record_bootstrap_file(record)
  store_read_object(store, file)
}

read_pattern <- function(name, record, meta, branches, path_store) {
  names <- record$children
  if (!is.null(branches)) {
    names <- names[branches]
  }
  if (length(diff <- setdiff(names, meta$name))) {
    diff <- if_any(anyNA(diff), "branches out of range", diff)
    tar_throw_validate(
      "branches not in metadata: ",
      paste(diff, collapse = ", ")
    )
  }
  meta <- meta[meta$name %in% names,, drop = FALSE] # nolint
  if (nrow(meta)) {
    meta <- meta[match(names, meta$name),, drop = FALSE] # nolint
  }
  records <- map_rows(meta, ~record_from_row(.x, path_store = path_store))
  objects <- lapply(records, read_builder)
  names(objects) <- names
  value <- value_init(iteration = record$iteration)
  value_produce_aggregate(value, objects)
}
