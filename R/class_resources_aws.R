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
  enclass(environment(), "tar_resources_aws")
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  assert_scalar(resources$bucket %|||% "bucket")
  assert_chr(resources$bucke %|||% "bucket")
  assert_nzchar(resources$bucket %|||% "bucket")
  assert_scalar(resources$prefix)
  assert_chr(resources$prefix)
  assert_nzchar(resources$prefix)
}

#' @export
print.tar_resources_aws <- function(x, ...) {
  cat(
    "<tar_resources_aws>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
