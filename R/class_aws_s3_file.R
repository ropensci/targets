store_new.aws_file <- function(class, file = NULL, resources = NULL) {
  aws_file_new(file = file, resources = resources)
}

aws_file_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_s3_file", "tar_aws_s3", "tar_store_file", "tar_store")
  )
}

#' @export
store_assert_format_setting.aws_file <- function(class) {
}

#' @export
store_produce_stage.tar_aws_s3_file <- function(store, name, object) {
  object
}

#' @export
store_read_object.tar_aws_s3_file <- function(store) {
  path <- store$file$path
  bucket <- store_aws_s3_bucket(path)
  key <- store_aws_s3_key(path)
  out <- path_scratch(pattern = basename(key))
  aws.s3::save_object(object = key, bucket = bucket, file = out)
  out
}
