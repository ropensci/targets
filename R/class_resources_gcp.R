resources_gcp_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud(),
  verbose = FALSE
) {
  resources_gcp_new(
    bucket = bucket,
    prefix = prefix,
    verbose = verbose
  )
}

resources_gcp_new <- function(
  bucket = NULL,
  prefix = NULL,
  verbose = FALSE
) {
  force(bucket)
  force(prefix)
  force(verbose)
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
  tar_assert_lgl(resources$verbose)
}

#' @export
print.tar_resources_gcp <- function(x, ...) {
  cat(
    "<tar_resources_gcp>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
