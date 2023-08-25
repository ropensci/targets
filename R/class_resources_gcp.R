resources_gcp_init <- function(
  bucket = NULL,
  prefix = tar_path_objects_dir_cloud(),
  predefined_acl = "private",
  max_tries = 5L,
  verbose = FALSE
) {
  resources_gcp_new(
    bucket = bucket,
    prefix = prefix,
    predefined_acl = predefined_acl,
    max_tries = max_tries,
    verbose = verbose
  )
}

resources_gcp_new <- function(
  bucket = NULL,
  prefix = NULL,
  predefined_acl = NULL,
  max_tries = NULL,
  verbose = NULL
) {
  force(bucket)
  force(prefix)
  force(predefined_acl)
  force(max_tries)
  force(verbose)
  enclass(environment(), c("tar_resources_gcp", "tar_resources"))
}

#' @export
resources_validate.tar_resources_gcp <- function(resources) {
  message <- "GCP resources require a valid bucket name."
  tar_assert_scalar(resources$bucket, msg = message)
  tar_assert_chr(resources$bucket, msg = message)
  tar_assert_none_na(resources$bucket, msg = message)
  tar_assert_nzchar(resources$bucket, msg = message)
  tar_assert_scalar(resources$prefix)
  tar_assert_chr(resources$prefix)
  tar_assert_nzchar(resources$prefix)
  tar_assert_scalar(resources$predefined_acl)
  tar_assert_chr(resources$predefined_acl)
  tar_assert_nzchar(resources$predefined_acl)
  tar_assert_scalar(resources$max_tries %|||% 1L)
  tar_assert_dbl(resources$max_tries %|||% 1L)
  tar_assert_none_na(resources$max_tries %|||% 1L)
  tar_assert_ge(resources$max_tries %|||% 1L, 0L)
  tar_assert_scalar(resources$verbose)
  tar_assert_lgl(resources$verbose)
}

#' @export
print.tar_resources_gcp <- function(x, ...) {
  cat(
    "<tar_resources_gcp>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
