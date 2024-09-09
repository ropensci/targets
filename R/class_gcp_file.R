# Semi-automated tests of GCP GCS integration live in tests/gcp/. # nolint
# These tests should not be fully automated because they
# automatically create buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_produce_path.tar_gcp_file <- function(store, name, object, path_store) {
  out <- store_produce_gcp_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
  c(out, paste0("stage=", object))
}

store_gcp_file_stage <- function(path) {
  store_gcp_path_field(path = path, pattern = "^stage=")
}

#' @export
store_produce_stage.tar_gcp_file <- function(store, name, object, path_store) {
  object
}

#' @export
store_assert_format_setting.gcp_file <- function(format) {
}

#' @export
store_upload_object.tar_gcp_file <- function(store) {
  store_upload_object_gcp(store)
}

#' @export
store_hash_early.tar_gcp_file <- function(store) { # nolint
  old <- store$file$path
  store$file$path <- store_gcp_file_stage(store$file$path)
  on.exit(store$file$path <- old)
  tar_assert_path(store$file$path)
  file_update_info(store$file)
}

#' @export
store_hash_late.tar_gcp_file <- function(store) { # nolint
}

#' @export
store_read_object.tar_gcp_file <- function(store) {
  path <- store$file$path
  key <- store_gcp_key(path)
  bucket <- store_gcp_bucket(path)
  scratch <- path_scratch_temp_network(pattern = basename(store_gcp_key(path)))
  dir_create(dirname(scratch))
  gcp_gcs_download(
    key = key,
    bucket = bucket,
    file = scratch,
    version = store_gcp_version(path),
    verbose = store$resources$gcp$verbose,
    max_tries = store$resources$gcp$max_tries
  )
  stage <- store_gcp_file_stage(path)
  dir_create(dirname(stage))
  file_move(from = scratch, to = stage)
  stage
}

#' @export
store_unload.tar_gcp_file <- function(store, target) {
  unlink(as.character(target$value$object))
  NextMethod()
}
# nocov end
