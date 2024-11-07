# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# Same for other potential cloud storage functionality.
# These tests should not be fully automated because they
# incur monetary costs for storage and API calls.
# nocov start
#' @export
store_tar_path.tar_cloud <- function(store, target, path_store) {
  target$file$stage
}

#' @export
store_produce_stage.tar_cloud <- function(store, name, object, path_store) {
  path_scratch_temp_network(pattern = name)
}

#' @export
store_write_object.tar_cloud <- function(store, file, object) {
  stage <- file$stage
  dir_create(dirname(stage))
  store_write_path(store, store_convert_object(store, object), stage)
}

#' @export
store_cache_path.tar_cloud <- function(store, path) {
}

#' @export
store_hash_late.tar_cloud <- function(store, file) {
  tar_assert_path(file$stage)
  file <- file_init(path = file$stage)
  file_update_info(file)
  file$bytes <- file$bytes
  file$time <- file$time
}

#' @export
store_ensure_correct_hash.tar_cloud <- function(
  store,
  file,
  storage,
  deployment
) {
}

#' @export
store_sync_file_meta.tar_cloud <- function(store, target, meta) {
}
# nocov end
