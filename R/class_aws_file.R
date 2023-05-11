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
      path_scratch(
        path_store = tempdir(),
        paste0("targets_aws_file_", basename(store_aws_key(path)))
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
  file_update_hash(store$file)
}

#' @export
store_read_object.tar_aws_file <- function(store) {
  path <- store$file$path
  key <- store_aws_key(path)
  bucket <- store_aws_bucket(path)
  scratch <- path_scratch(
    path_store = tempdir(),
    pattern = basename(store_aws_key(path))
  )
  dir_create(dirname(scratch))
  seconds_interval <- store$resources$network$seconds_interval %|||% 1
  seconds_timeout <- store$resources$network$seconds_timeout %|||% 30
  max_tries <- store$resources$network$max_tries %|||% Inf
  verbose <- store$resources$network$verbose %|||% TRUE
  retry_until_true(
    ~{
      aws_s3_download(
        key = key,
        bucket = bucket,
        file = scratch,
        region = store_aws_region(path),
        version = store_aws_version(path),
        args = store$resources$aws$args
      )
      TRUE
    },
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    catch_error = TRUE,
    message = sprintf("Cannot download object %s from bucket %s", key, bucket),
    verbose = verbose
  )
  stage <- store_aws_file_stage(path)
  dir_create(dirname(stage))
  file.rename(from = scratch, to = stage)
  stage
}

#' @export
store_unload.tar_aws_file <- function(store, target) {
  unlink(as.character(target$value$object))
}
# nocov end
