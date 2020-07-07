store_init <- function(format = "rds") {
  file <- file_init()
  switch(
    format,
    file = store_file_new(file),
    rds = rds_new(file),
    qs = qs_new(file),
    keras = keras_new(file),
    fst = fst_new(file),
    fst_dt = fst_dt_new(file),
    fst_tbl = fst_tbl_new(file),
    throw_validate("unsupported format")
  )
}

store_new <- function(file = NULL) {
  force(file)
  enclass(environment(), "tar_store")
}

store_formats <- function() {
  c("file", "rds", "qs", "keras", "fst", "fst_dt", "fst_tbl")
}

store_read_object <- function(store) {
  store_coerce_object(store, store_read_path(store, store$file$path))
}

store_read_path <- function(store, path) {
  UseMethod("store_read_path")
}

store_write_object <- function(store, object) {
  UseMethod("store_write_object")
}

#' @export
store_write_object.default <- function(store, object) {
  path <- store$file$path
  dir_create(dirname(path))
  tmp <- store_path_scratch(pattern = basename(path))
  dir_create(dirname(tmp))
  store_write_path(store, store_coerce_object(store, object), tmp)
  file.rename(tmp, path)
}

store_write_path <- function(store, object, path) {
  UseMethod("store_write_path")
}

store_update_path <- function(store, name, object) {
  store$file$path <- store_produce_path(store, name, object)
}

store_produce_path <- function(store, name, object) {
  UseMethod("store_produce_path")
}

#' @export
store_produce_path.default <- function(store, name, object) {
  store_path_default(name)
}

store_coerce_object <- function(store, object) {
  store_assert_format(store, object)
  UseMethod("store_coerce_object")
}

store_coerce_object.default <- function(store, object) {
  object
}

store_assert_format <- function(store, object) {
  UseMethod("store_assert_format")
}

#' @export
store_assert_format.default <- function(store, object) {
}

store_early_hash <- function(store) {
  UseMethod("store_early_hash")
}

#' @export
store_early_hash.default <- function(store) {
}

store_late_hash <- function(store) {
  UseMethod("store_late_hash")
}

#' @export
store_late_hash.default <- function(store) {
  file_update_hash(store$file)
}

store_wait_correct_hash <- function(store, remote) {
  UseMethod("store_wait_correct_hash")
}

#' @export
store_wait_correct_hash.default <- function(store, remote) {
  if (remote) {
    file_wait_correct_hash(store$file)
  }
}

store_serialize_value <- function(store, value) {
  UseMethod("store_serialize_value")
}

store_serialize_value.default <- function(store, value) {
}

store_unserialize_value <- function(store, value) {
  UseMethod("store_unserialize_value")
}

#' @export
store_unserialize_value.default <- function(store, value) {
}

store_validate <- function(store) {
  assert_correct_fields(store, store_new)
  store_validate_packages(store)
}

store_validate_packages <- function(store) {
  UseMethod("store_validate_packages")
}

#' @export
store_validate_packages.default <- function(store) {
}

store_path_default <- function(name) {
  file.path("_targets", "objects", name)
}

store_path_scratch <- function(pattern = "") {
  tempfile(pattern = pattern, tmpdir = store_dir_scratch())
}

store_dir_scratch <- function() {
  file.path("_targets", "scratch")
}

store_del_scratch <- function() {
  unlink(store_dir_scratch(), recursive = TRUE)
}
