#' @export
store_new.aws_file <- function(class, file = NULL, resources = NULL) {
  aws_file_new(file = file, resources = resources)
}

aws_file_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_file", "tar_aws", "tar_external", "tar_store_file", "tar_store")
  )
}

# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_produce_stage.tar_aws_file <- function(store, name, object) {
  object
}

#' @export
store_assert_format_setting.aws_file <- function(class) {
}

#' @export
store_read_object.tar_aws_file <- function(store) {
  path <- store$file$path
  bucket <- store_aws_bucket(path)
  key <- store_aws_key(path)
  out <- path_scratch_fixed(name = basename(key))
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
