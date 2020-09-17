#' @export
store_new.file <- function(class, file) {
  store_file_new(file)
}

store_file_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_store_file", "tar_store"))
}

#' @export
store_assert_format_setting.file <- function(class) {
}

#' @export
store_read_path.tar_store_file <- function(store, path) {
  path
}

#' @export
store_write_object.tar_store_file <- function(store, object) {
}

#' @export
store_write_path.tar_store_file <- function(store, object, path) {
}

#' @export
store_produce_path.tar_store_file <- function(store, target) { # nolint
  target$value$object
}

#' @export
store_coerce_object.tar_store_file <- function(store, object) { # nolint
  as.character(object)
}

#' @export
store_assert_format.tar_store_file <- function(store, object) { # nolint
  if (!is.null(object) && !is.character(object)) {
    throw_validate(
      "dynamic files (targets with format = \"file\") must return ",
      "character vectors of file or directory paths."
    )
  }
}

#' @export
store_early_hash.tar_store_file <- function(store) { # nolint
  file_update_hash(store$file)
}

#' @export
store_late_hash.tar_store_file <- function(store) { # nolint
}

#' @export
store_wait_correct_hash.tar_store_file <- function(store, remote) { # nolint
  file_wait_correct_hash(store$file)
}
