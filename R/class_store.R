store_init <- function(
  format = "rds",
  repository = "local",
  resources = list()
) {
  store <- store_new(
    resources = resources,
    methods_format = store_methods_format(format),
    methods_repository = store_methods_repository(repository)
  )
  store_enclass(store, format = format, repository = repository)
}

store_mock <- function(
  format = "rds",
  repository = "local"
) {
  mock <- list(
    methods_format = store_methods_format(format),
    methods_repository = store_methods_repository(repository)
  )
  store_enclass(mock, format = format, repository = repository)
}

store_new <- function(
  resources = NULL,
  methods_format = NULL,
  methods_repository = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$resources <- resources
  out$methods_format <- methods_format
  out$methods_repository <- methods_repository
  out
}

store_enclass <- function(store, format, repository) {
  class(store) <- store_class_format(store_dispatch_format(format))
  class(store) <- store_class_repository(
    repository = store_dispatch_repository(repository),
    store = store,
    format = format
  )
  store
}

# A format should not be a full class like the store
# because the responsibilities of store and format
# would overlap too much.
store_dispatch_format <- function(format) {
  if (is_format_custom(format)) {
    class <- "format_custom"
  } else {
    class <- format
  }
  enclass(format, class)
}

store_dispatch_repository <- function(repository) {
  if (is_repository_cas(repository)) {
    class <- "repository_cas"
  } else {
    class <- repository
  }
  enclass(repository, class)
}

store_methods_format <- function(format) {
  if_any(
    is_format_custom(format),
    store_format_custom_methods_init(format),
    NULL
  )
}

store_methods_repository <- function(repository) {
  if_any(
    is_repository_cas(repository),
    store_repository_cas_methods_init(repository),
    NULL
  )
}

is_format_custom <- function(format) {
  !is.null(format) && grepl(pattern = "^format_custom", x = format)
}

is_repository_cas <- function(repository) {
  !is.null(repository) && grepl(pattern = "^repository_cas", x = repository)
}

store_class_format <- function(format) {
  UseMethod("store_class_format")
}

store_class_repository <- function(repository, store, format) {
  UseMethod("store_class_repository")
}

#' @export
store_class_repository.default <- function(repository, store, format) {
  class(store)
}

store_assert_format_setting <- function(format) {
  UseMethod("store_assert_format_setting")
}

#' @export
store_assert_format_setting.default <- function(format) {
  tar_throw_validate(paste("unsupported format:", class(format)[1]))
}

store_assert_repository_setting <- function(repository) {
  UseMethod("store_assert_repository_setting")
}

#' @export
store_assert_repository_setting.default <- function(repository) {
  tar_throw_validate(paste("unsupported repository:", repository))
}

#' @export
store_assert_repository_setting.local <- function(repository) {
}

store_read_object <- function(store, file) {
  UseMethod("store_read_object")
}

#' @export
store_read_object.default <- function(store, file) {
  store_convert_object(store, store_read_path(store, file$path))
}

store_read_path <- function(store, path) {
  UseMethod("store_read_path")
}

store_write_object <- function(store, file, object) {
  UseMethod("store_write_object")
}

#' @export
store_write_object.default <- function(store, file, object) {
  path <- file$path
  stage <- file$stage
  dir_create_runtime(dirname(path))
  dir_create_runtime(dirname(stage))
  store_write_path(store, store_convert_object(store, object), stage)
  file_move(from = stage, to = path)
}

store_write_path <- function(store, object, path) {
  UseMethod("store_write_path")
}

store_cache_path <- function(store, path) {
  UseMethod("store_cache_path")
}

#' @export
store_cache_path.default <- function(store, path) {
  cache <- tar_runtime$file_exist
  if (!is.null(cache)) {
    counter_set_names(counter = cache, names = path)
  }
}

store_exist_object <- function(store, file, name = NULL) {
  UseMethod("store_exist_object")
}

#' @export
store_exist_object.default <- function(store, file, name = NULL) {
  all(file.exists(file$path))
}

store_delete_object <- function(store, file, name = NULL) {
  UseMethod("store_delete_object")
}

#' @export
store_delete_object.default <- function(store, file, name = NULL) {
  unlink(file$path)
  unlink(file$stage)
}

store_delete_objects <- function(store, meta, batch_size, verbose) {
  UseMethod("store_delete_objects")
}

#' @export
store_delete_objects.default <- function(store, meta, batch_size, verbose) {
  tar_throw_validate(
    "store_delete_objects() is for supported cloud objects only."
  )
}

store_upload_object <- function(store, file) {
  UseMethod("store_upload_object")
}

#' @export
store_upload_object.default <- function(store, file) {
}

store_update_path <- function(store, file, name, object, path_store) {
  file$path <- store_produce_path(store, name, object, path_store)
}

store_produce_path <- function(store, name, object, path_store) {
  UseMethod("store_produce_path")
}

#' @export
store_produce_path.default <- function(store, name, object, path_store) {
  path_objects(path_store = path_store, name = name)
}

store_row_path <- function(store, file) {
  UseMethod("store_row_path")
}

#' @export
store_row_path.default <- function(store, file) {
  NA_character_
}

store_path_from_record <- function(store, record, path_store) {
  UseMethod("store_path_from_record")
}

#' @export
store_path_from_record.default <- function(store, record, path_store) {
  path_objects(path_store = path_store, name = record$name)
}

store_tar_path <- function(store, target, path_store) {
  UseMethod("store_tar_path")
}

#' @export
store_tar_path.default <- function(store, target, path_store) {
  path_objects(path_store = path_store, name = target_get_name(target))
}

store_update_stage_early <- function(store, file, name, path_store) {
  UseMethod("store_update_stage_early")
}

#' @export
store_update_stage_early.default <- function(store, file, name, path_store) {
  file$stage <- store_produce_stage(
    store = store,
    name = name,
    object = NULL,
    path_store = path_store
  )
}

store_update_stage_late <- function(store, file, name, object, path_store) {
  UseMethod("store_update_stage_late")
}

#' @export
store_update_stage_late.default <- function(
  store,
  file,
  name,
  object,
  path_store
) {
}

#' @export
store_update_stage_early.default <- function(store, file, name, path_store) {
  file$stage <- store_produce_stage(
    store = store,
    name = name,
    object = NULL,
    path_store = path_store
  )
}

store_produce_stage <- function(store, name, object, path_store) {
  UseMethod("store_produce_stage")
}

#' @export
store_produce_stage.default <- function(store, name, object, path_store) {
  path_scratch(path_store = path_store, pattern = name)
}

store_convert_object <- function(store, object) {
  UseMethod("store_convert_object")
}

store_convert_object.default <- function(store, object) {
  object
}

store_copy_object <- function(store, object) {
  UseMethod("store_copy_object")
}

store_copy_object.default <- function(store, object) {
  object
}

store_assert_format <- function(store, object, name) {
  UseMethod("store_assert_format")
}

#' @export
store_assert_format.default <- function(store, object, name) {
}

store_hash_early <- function(store, file) {
  UseMethod("store_hash_early")
}

#' @export
store_hash_early.default <- function(store, file) {
}

store_hash_late <- function(store, file) {
  UseMethod("store_hash_late")
}

#' @export
store_hash_late.default <- function(store, file) {
  tar_assert_path(file$path)
  file_update_hash(file)
}

store_ensure_correct_hash <- function(
  store,
  file,
  storage,
  deployment
) {
  UseMethod("store_ensure_correct_hash")
}

#' @export
store_ensure_correct_hash.default <- function(
  store,
  file,
  storage,
  deployment
) {
  if (identical(storage, "worker") && identical(deployment, "worker")) {
    store_wait_correct_hash(store, file)
  }
}

store_wait_correct_hash <- function(store, file) {
  seconds_interval <- store$resources$network$seconds_interval %|||% 0.25
  seconds_timeout <- store$resources$network$seconds_timeout %|||% 60
  max_tries <- store$resources$network$max_tries %|||% Inf
  verbose <- store$resources$network$verbose %|||% TRUE
  retry_until_true(
    fun = ~store_has_correct_hash(store, file),
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    catch_error = FALSE,
    message = paste(
      "Path",
      paste(file$path, collapse = " "),
      "does not exist or has incorrect hash.",
      "File sync timed out."
    ),
    verbose = verbose
  )
}

store_has_correct_hash <- function(store, file) {
  UseMethod("store_has_correct_hash")
}

#' @export
store_has_correct_hash.default <- function(store, file) {
  (all(is.na(file$path)) || file_exists_path(file)) &&
    file_has_correct_hash(file)
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
  info <- file_info_runtime(target$file$path)
  time <- file_time(info)
  bytes <- file_bytes(info)
  size <- file_size(bytes)
  # Fully automated tests do no use big files.
  # Tested in tests/interactive/test-file.R. # nolint
  # nocov start
  if (!identical(time, file$time) || !identical(size, file$size)) {
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
  target$value <- NULL
}

store_marshal_object <- function(store, object) {
  UseMethod("store_marshal_object")
}

#' @export
store_marshal_object.default <- function(store, object) {
  object
}

store_unmarshal_object <- function(store, object) {
  UseMethod("store_unmarshal_object")
}

#' @export
store_unmarshal_object.default <- function(store, object) {
  object
}

store_marshal_value <- function(store, target) {
  UseMethod("store_marshal_value")
}

#' @export
store_marshal_value.default <- function(store, target) {
}

store_unmarshal_value <- function(store, target) {
  UseMethod("store_unmarshal_value")
}

#' @export
store_unmarshal_value.default <- function(store, target) {
}

store_validate <- function(store) {
  tar_assert_correct_fields(store, store_new)
  store_validate_packages(store)
  tar_assert_list(store$resources)
  if (!is.null(store$methods_format)) {
    store_format_custom_methods_validate(store$methods_format)
  }
  if (!is.null(store$methods_repository)) {
    store_repository_cas_methods_validate(store$methods_repository)
  }
}

store_validate_packages <- function(store) {
  tar_assert_package(store_get_packages(store))
}

store_get_packages <- function(store) {
  UseMethod("store_get_packages")
}

#' @export
store_get_packages.default <- function(store) {
  character(0)
}
