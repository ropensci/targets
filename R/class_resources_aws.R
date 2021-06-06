resources_init <- function(
  bucket = character(0),
  prefix = path_objects_dir_cloud()
) {
  resources_new(
    bucket = bucket,
    prefix = prefix
  )
}

resources_new <- function(
  bucket = NULL,
  prefix = NULL
) {
  force(bucket)
  force(prefix)
  enclass(environment(), "tar_resources_aws")
}

#' @export
resources_validate.tar_resources_aws <- function(resources) {
  assert_scalar(resources$bucket)
  assert_chr(resources$bucket)
  assert_nzchar(resources$bucket)
  assert_scalar(resources$prefix)
  assert_chr(resources$prefix)
  assert_nzchar(resources$prefix)
}

#' @export
print.tar_resources_aws <- function(x, ...) {
  cat("<aws>\n ", paste0(paste_list(as.list(x)), collapse = "\n  "))
}
