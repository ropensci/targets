resources_gcp_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud()
) {
  resources_gcp_new(
    bucket = bucket,
    prefix = prefix
  )
}

resources_gcp_new <- function(
  bucket = NULL,
  prefix = NULL
) {
  force(bucket)
  force(prefix)
  enclass(environment(), c("tar_resources_gcp", "tar_resources"))
}

#' @export
resources_validate.tar_resources_gcp <- function(resources) {
  tar_assert_scalar(resources$bucket %|||% "bucket")
  tar_assert_chr(resources$bucket %|||% "bucket")
  tar_assert_nzchar(resources$bucket %|||% "bucket")
  tar_assert_scalar(resources$prefix)
  tar_assert_chr(resources$prefix)
  tar_assert_nzchar(resources$prefix)
}

#' @export
print.tar_resources_gcp <- function(x, ...) {
  cat(
    "<tar_resources_gcp>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
