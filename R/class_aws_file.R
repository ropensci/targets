# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_produce_path.tar_aws_file <- function(store, name, object, path_store) {
  out <- store_produce_aws_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
  c(out, paste0("stage=", object))
}

store_aws_file_stage <- function(path) {
  if_any(
    store_aws_path_0.8.1(path), # targets 0.8.1 and under
    if_any(
      length(path) <= 2L, # targets 0.4.2 and under
      path_scratch_temp_network(
        pattern = paste0("targets_aws_file_", basename(store_aws_key(path)))
      ),
      path[3]
    ),
    store_aws_path_field(path = path, pattern = "^stage=")
  )
}

#' @export
store_produce_stage.tar_aws_file <- function(store, name, object, path_store) {
  object
}

#' @export
store_assert_format_setting.aws_file <- function(format) {
}

#' @export
store_upload_object.tar_aws_file <- function(store) {
  store_upload_object_aws(store)
}

#' @export
store_hash_early.tar_aws_file <- function(store) { # nolint
  old <- store$file$path
  store$file$path <- store_aws_file_stage(store$file$path)
  on.exit(store$file$path <- old)
  tar_assert_path(store$file$path)
  file_update_info(store$file)
}

#' @export
store_hash_late.tar_aws_file <- function(store) { # nolint
}

#' @export
store_read_object.tar_aws_file <- function(store) {
  path <- store$file$path
  key <- store_aws_key(path)
  bucket <- store_aws_bucket(path)
  scratch <- path_scratch_temp_network(pattern = basename(store_aws_key(path)))
  dir_create(dirname(scratch))
  aws <- store$resources$aws
  aws_s3_download(
    key = key,
    bucket = bucket,
    file = scratch,
    region = store_aws_region(path),
    endpoint = store_aws_endpoint(path),
    version = store_aws_version(path),
    args = aws$args,
    max_tries = aws$max_tries,
    seconds_timeout = aws$seconds_timeout,
    close_connection = aws$close_connection,
    s3_force_path_style = aws$s3_force_path_style
  )
  stage <- store_aws_file_stage(path)
  dir_create(dirname(stage))
  file_move(from = scratch, to = stage)
  stage
}

#' @export
store_unload.tar_aws_file <- function(store, target) {
  unlink(as.character(target$value$object))
}
# nocov end
