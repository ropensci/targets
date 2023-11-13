resources_gcp_init <- function(
  bucket = NULL,
  prefix = path_objects_dir_cloud(),
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
  for (field in c("bucket", "prefix", "predefined_acl")) {
    msg <- paste("invalid GCP GCS", field)
    tar_assert_scalar(resources[[field]], msg = msg)
    tar_assert_chr(resources[[field]], msg = msg)
    tar_assert_none_na(resources[[field]], msg = msg)
    tar_assert_nzchar(resources[[field]], msg = msg)
  }
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
