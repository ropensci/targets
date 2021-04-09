#' @export
store_new.aws_parquet <- function(class, file = NULL, resources = NULL) {
  aws_parquet_new(file = file, resources = resources)
}

aws_parquet_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_parquet", "tar_aws", "tar_external", "tar_parquet", "tar_store")
  )
}

#' @export
store_assert_format_setting.aws_parquet <- function(class) {
}
