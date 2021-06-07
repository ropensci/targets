resources_aws_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud()
) {
  resources_aws_new(
    bucket = bucket,
    prefix = prefix
  )
}

resources_aws_new <- function(
  bucket = NULL,
  prefix = NULL
) {
  force(bucket)
  force(prefix)
  enclass(environment(), c("tar_resources_aws", "tar_resources"))
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  assert_scalar(resources$bucket %|||% "bucket", "bucket must have length 1.")
  assert_chr(resources$bucket %|||% "bucket must be character.")
  assert_nzchar(resources$bucket %|||% "bucket name must be nonempty.")
  assert_scalar(resources$prefix, "prefix must have length 1.")
  assert_chr(resources$prefix, "prefix must be character.")
  assert_nzchar(resources$prefix, "prefix must be nonempty.")
}

#' @export
print.tar_resources_aws <- function(x, ...) {
  cat(
    "<tar_resources_aws>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
