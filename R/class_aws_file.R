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
  c(out, paste0("stage=", object))
}

store_aws_file_stage <- function(path) {
  if_any(
    store_aws_path_0.8.1(path), # targets 0.8.1 and under
    if_any(
      length(path) <= 2L, # targets 0.4.2 and under
      file.path(path_scratch_dir(path_store_default()), store_aws_key(path)),
      path[3]
    ),
    store_aws_field(path = path, pattern = "^stage=")
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
  old <- store$file$path
  store$file$path <- store_aws_file_stage(store$file$path)
  on.exit(store$file$path <- old)
  tar_assert_path(store$file$path)
  file_update_hash(store$file)
}

#' @export
store_read_object.tar_aws_file <- function(store) {
  path <- store$file$path
  stage <- store_aws_file_stage(path)
  dir_create(dirname(stage))
  aws_s3_download(
    key = store_aws_key(path),
    bucket = store_aws_bucket(path),
    file = stage,
    region = store_aws_region(path),
    version = store_aws_version(path)
  )
  stage
}

#' @export
store_unload.tar_aws_file <- function(store, target) {
  unlink(as.character(target$value$object))
}
# nocov end
