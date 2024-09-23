#' @export
store_class_format.file <- function(format) {
  c("tar_store_file", "tar_external", "tar_store")
}

#' @export
store_assert_format_setting.file <- function(format) {
}

#' @export
store_assert_format_setting.file_fast <- function(format) {
}

#' @export
store_read_path.tar_store_file <- function(store, path) {
  path[!is.na(path)]
}

#' @export
store_write_object.tar_store_file <- function(store, object) {
}

#' @export
store_write_path.tar_store_file <- function(store, object, path) {
}

#' @export
store_produce_path.tar_store_file <- function( # nolint
  store,
  name,
  object,
  path_store
) {
  object
}

#' @export
store_convert_object.tar_store_file <- function(store, object) { # nolint
  as.character(object)
}

#' @export
store_assert_format.tar_store_file <- function(store, object, name) { # nolint
  if (!is.character(object %|||% character(0))) {
    tar_throw_validate(
      "target ", name, " did not return a character. ",
      "dynamic files (targets with format = \"file\") must return ",
      "character vectors of file or directory paths."
    )
  }
}

#' @export
store_update_stage_early.tar_store_file <- function(store, name, path_store) {
}

#' @export
store_update_stage_late.tar_store_file <- function(
  store,
  name,
  object,
  path_store
) {
  store$file$stage <- store_produce_stage(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

#' @export
store_hash_early.tar_store_file <- function(store) { # nolint
  tar_assert_path(store$file$path)
  file_update_hash(store$file)
}

#' @export
store_hash_late.tar_store_file <- function(store) { # nolint
}

#' @export
store_ensure_correct_hash.tar_store_file <- function(
  store,
  storage,
  deployment
) {
  if_any(
    identical(deployment, "worker"),
    store_wait_correct_hash(store),
    tar_assert_path(store$file$path)
  )
}
