resources_aws_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud(),
  region = NULL,
  part_size = 5 * (2 ^ 20)
) {
  resources_aws_new(
    bucket = bucket,
    prefix = prefix,
    region = region,
    part_size = part_size
  )
}

resources_aws_new <- function(
  bucket = NULL,
  prefix = NULL,
  region = NULL,
  part_size = NULL
) {
  force(bucket)
  force(prefix)
  force(region)
  force(part_size)
  enclass(environment(), c("tar_resources_aws", "tar_resources"))
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  tar_assert_scalar(resources$bucket %|||% "bucket")
  tar_assert_chr(resources$bucket %|||% "bucket")
  tar_assert_nzchar(resources$bucket %|||% "bucket")
  tar_assert_scalar(resources$prefix)
  tar_assert_chr(resources$prefix)
  tar_assert_nzchar(resources$prefix)
  tar_assert_scalar(resources$region %|||% "region")
  tar_assert_chr(resources$region %|||% "region")
  tar_assert_scalar(resources$part_size %|||% 1e8)
  tar_assert_dbl(resources$part_size %|||% 1e8)
  tar_assert_positive(resources$part_size %|||% 1e8)
}

#' @export
print.tar_resources_aws <- function(x, ...) {
  cat(
    "<tar_resources_aws>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
