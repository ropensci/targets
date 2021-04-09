#' @export
store_new.aws_qs <- function(class, file = NULL, resources = NULL) {
  aws_qs_new(file = file, resources = resources)
}

aws_qs_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_aws_qs", "tar_aws", "tar_external", "tar_qs", "tar_store")
  )
}

#' @export
store_assert_format_setting.aws_qs <- function(class) {
}
