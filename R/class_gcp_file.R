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
  file_update_hash(store$file)
}

#' @export
store_read_object.tar_gcp_file <- function(store) {
  path <- store$file$path
  scratch <- path_scratch(
    path_store = tempdir(),
    pattern = "targets_gcp_file_"
  )
  dir_create(dirname(scratch))
  gcp_gcs_download(
    key = store_gcp_key(path),
    bucket = store_gcp_bucket(path),
    file = scratch,
    version = store_gcp_version(path),
    verbose = store$resources$gcp$verbose %|||% FALSE
  )
  stage <- store_gcp_file_stage(path)
  dir_create(dirname(stage))
  file.rename(from = scratch, to = stage)
  stage
}

#' @export
store_unload.tar_gcp_file <- function(store, target) {
  unlink(as.character(target$value$object))
}
# nocov end
