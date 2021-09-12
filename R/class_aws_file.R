#' @export
store_new.aws_file <- function(class, file = NULL, resources = NULL) {
  aws_file_new(file = file, resources = resources)
}

aws_file_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c(
      "tar_aws_file",
      "tar_aws",
      "tar_cloud",
      "tar_external",
      "tar_store_file",
      "tar_store"
    )
  )
}

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
  c(out, object)
}

store_aws_file_stage <- function(path, key) {
  if_any(
    length(path) > 2L,
    path[3],
    # For compatibility with version 0.4.2 and under:
    file.path(path_scratch_dir(path_store_default()), basename(key))
  )
}

#' @export
store_produce_stage.tar_aws_file <- function(store, name, object, path_store) {
  object
}

#' @export
store_assert_format_setting.aws_file <- function(class) {
}

#' @export
store_hash_early.tar_aws_file <- function(store, target) { # nolint
  tar_assert_path(store_aws_path(store$file$path))
  file_update_hash(store$file)
}

#' @export
store_read_object.tar_aws_file <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  key <- store_aws_key(path)
  out <- store_aws_file_stage(path, key)
  dir_create(dirname(out))
  aws.s3::save_object(
    object = key,
    bucket = bucket,
    file = out,
    check_region = TRUE
  )
  out
}

#' @export
store_unload.tar_aws_file <- function(store, target) {
  unlink(as.character(target$value$object))
}
# nocov end
