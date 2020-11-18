store_new.aws_rds <- function(class, file = NULL, resources = NULL) {
  aws_rds_new(file = file, resources = resources)
}

aws_rds_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_rds", "tar_aws", "tar_rds", "tar_store")
  )
}

#' @export
store_assert_format_setting.aws_rds <- function(class) {
}
