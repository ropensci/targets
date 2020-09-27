store_init <- function(format = "rds", resources = list()) {
  store_new(
    class = as_class(format),
    file = file_init(),
    resources = resources
  )
}

store_new <- function(class, file = NULL, resources = NULL) {
  UseMethod("store_new")
}

#' @export
store_new.default <- function(class, file = NULL, resources = NULL) {
  store_new_default(file, resources)
}

store_new_default <- function(file, resources) {
  force(file)
  force(resources)
  enclass(environment(), "tar_store")
}

store_assert_format_setting <- function(class) {
  UseMethod("store_assert_format_setting")
}

#' @export
store_assert_format_setting.default <- function(class) {
  throw_validate("unsupported format")
}

store_read_object <- function(store) {
  UseMethod("store_read_object")
}

store_read_object.default <- function(store) {
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
  stage <- store$file$stage
  dir_create(dirname(path))
  dir_create(dirname(stage))
  store_write_path(store, store_coerce_object(store, object), stage)
  file.rename(stage, path)
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
  path_default(name)
}

store_update_stage <- function(store, name, object) {
  store$file$stage <- store_produce_stage(store, name, object)
}

store_produce_stage <- function(store, name, object) {
  UseMethod("store_produce_stage")
}

#' @export
store_produce_stage.default <- function(store, name, object) {
  path_scratch(pattern = name)
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
  assert_correct_fields(store, store_new_default)
  store_validate_packages(store)
  assert_list(store$resources)
}

store_validate_packages <- function(store) {
  UseMethod("store_validate_packages")
}

#' @export
store_validate_packages.default <- function(store) {
}

store_warn_output <- function(store, name) {
  UseMethod("store_warn_output")
}

#' @export
store_warn_output.default <- function(store, name) {
  warn_output(name, store$file$path)
}

store_has_correct_hash <- function(store, file) {
  UseMethod("store_has_correct_hash")
}

#' @export
store_has_correct_hash.default <- function(store, file) {
  all(file.exists(file$path)) && file_has_correct_hash(file)
}
