#' @export
store_class_format.file <- function(format) {
  store_class_format_file
}

#' @export
store_class_format.file_fast <- function(format) {
  store_class_format_file
}

store_class_format_file <- c("tar_store_file", "tar_external", "tar_store")

#' @export
store_assert_format_setting.file <- function(format) {}

#' @export
store_assert_format_setting.file_fast <- function(format) {}

#' @export
store_read_path.tar_store_file <- function(store, path) {
  path[!is.na(path)]
}

#' @export
store_write_object.tar_store_file <- function(store, file, object) {}

#' @export
store_write_path.tar_store_file <- function(store, object, path) {}

#' @export
store_produce_path.tar_store_file <- function(
  # nolint
  store,
  name,
  object,
  path_store
) {
  object
}

#' @export
store_convert_object.tar_store_file <- function(store, object) {
  # nolint
  as.character(object)
}

#' @export
store_assert_format.tar_store_file <- function(store, object, name) {
  # nolint
  if (!is.character(object %|||% character(0))) {
    tar_throw_validate(
      "target ",
      name,
      " did not return a character. ",
      "File targets (targets with format = \"file\") must return ",
      "character vectors of file or directory paths."
    )
  }
}

#' @export
store_update_stage_early.tar_store_file <- function(
  store,
  file,
  name,
  path_store
) {}

#' @export
store_update_stage_late.tar_store_file <- function(
  store,
  file,
  name,
  object,
  path_store
) {
  file$stage <- store_produce_stage(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

#' @export
store_hash_early.tar_store_file <- function(store, file) {
  # nolint
  tar_assert_path(file$path)
  file_update_hash(file)
}

#' @export
store_hash_late.tar_store_file <- function(store, file) {
  # nolint
}

#' @export
store_ensure_correct_hash.tar_store_file <- function(
  store,
  file,
  storage,
  deployment
) {
  if_any(
    identical(deployment, "worker"),
    store_wait_correct_hash(store, file),
    tar_assert_path(file$path)
  )
}
