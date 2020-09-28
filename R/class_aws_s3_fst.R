store_new.aws_fst <- function(class, file = NULL, resources = NULL) {
  aws_fst_new(file = file, resources = resources)
}

aws_fst_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_s3_fst", "tar_aws_s3", "tar_fst", "tar_store")
  )
}

#' @export
store_assert_format_setting.aws_fst <- function(class) {
}
