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

store_upload_object <- function(store) {
  UseMethod("store_upload_object")
}

store_upload_object.default <- function(store) {
}

store_update_path <- function(store, name, object) {
  store$file$path <- store_produce_path(store, name, object)
}

store_produce_path <- function(store, name, object) {
  UseMethod("store_produce_path")
}

#' @export
store_produce_path.default <- function(store, name, object) {
  path_objects(name)
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

store_ensure_correct_hash <- function(
  store,
  storage,
  deployment
) {
  UseMethod("store_ensure_correct_hash")
}

#' @export
store_ensure_correct_hash.default <- function(store, storage, deployment) {
  if (identical(storage, "worker") && identical(deployment, "worker")) {
    store_wait_correct_hash(store)
  }
}

store_wait_correct_hash <- function(store, sleep = 0.01, timeout = 60) {
  time_left <- timeout
  while (time_left > 0) {
    if (store_has_correct_hash(store)) {
      return(invisible())
    }
    Sys.sleep(sleep)
    time_left <- time_left - sleep
  }
  msg <- paste(
    "Path",
    paste(store$file$path, collapse = " "),
    "does not exist or has incorrect hash.",
    "File sync timed out."
  )
  throw_file(msg)
}

store_has_correct_hash <- function(store) {
  UseMethod("store_has_correct_hash")
}

#' @export
store_has_correct_hash.default <- function(store) {
  all(file.exists(store$file$path)) && file_has_correct_hash(store$file)
}

store_sync_file_meta <- function(store, target, meta) {
  UseMethod("store_sync_file_meta")
}

#' @export
store_sync_file_meta.default <- function(store, target, meta) {
  cue <- target$cue
  if (identical(cue$mode, "never") || identical(cue$file, FALSE)) {
    return()
  }
  name <- target_get_name(target)
  record <- meta$get_record(name)
  file <- file_init(
    path = record$path,
    time = record$time,
    size = record$size,
    bytes = record$bytes
  )
  info <- file_info(target$store$file$path)
  time <- file_time(info)
  bytes <- file_bytes(info)
  size <- file_size(bytes)
  sync <- file_should_rehash(
    file = file,
    time = time,
    size = size,
    bytes = bytes
  )
  # Fully automated tests do no use big files.
  # Tested in tests/interactive/test-file.R. # nolint
  # nocov start
  if (sync) {
    record$time <- time
    record$size <- size
    record$bytes <- bytes
    meta$insert_record(record)
  }
  # nocov end
}

store_unload <- function(store, target) {
  UseMethod("store_unload")
}

#' @export
store_unload.default <- function(store, target) {
}

store_serialize_object <- function(store, object) {
  UseMethod("store_serialize_object")
}

#' @export
store_serialize_object.default <- function(store, object) {
  object
}

store_unserialize_object <- function(store, object) {
  UseMethod("store_unserialize_object")
}

#' @export
store_unserialize_object.default <- function(store, object) {
  object
}

store_serialize_value <- function(store, target) {
  UseMethod("store_serialize_value")
}

#' @export
store_serialize_value.default <- function(store, target) {
}

store_unserialize_value <- function(store, target) {
  UseMethod("store_unserialize_value")
}

#' @export
store_unserialize_value.default <- function(store, target) {
}

store_validate <- function(store) {
  assert_correct_fields(store, store_new_default)
  store_validate_packages(store)
  assert_list(store$resources)
}

store_validate_packages <- function(store) {
  map(store_get_packages(store), assert_package)
}

store_get_packages <- function(store) {
  UseMethod("store_get_packages")
}

#' @export
store_get_packages.default <- function(store) {
  character(0)
}
