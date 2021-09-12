# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# Same for other potential cloud storage functionality.
# These tests should not be fully automated because they
# incur monetary costs for storage and API calls.
# nocov start
#' @export
store_tar_path.tar_cloud <- function(store, target, path_store) {
  store$file$stage
}

#' @export
store_write_object.tar_cloud <- function(store, object) {
  stage <- store$file$stage
  dir_create(dirname(stage))
  store_write_path(store, store_cast_object(store, object), stage)
}

#' @export
store_hash_late.tar_cloud <- function(store) {
  tar_assert_path(store$file$stage)
  file <- file_init(path = store$file$stage)
  file_update_hash(file)
  store$file$hash <- file$hash
  store$file$bytes <- file$bytes
  store$file$time <- file$time
}

#' @export
store_ensure_correct_hash.tar_cloud <- function(store, storage, deployment) {
  store_wait_correct_hash(store)
}

#' @export
store_sync_file_meta.tar_cloud <- function(store, target, meta) {
}
# nocov end
