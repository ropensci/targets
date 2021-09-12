#' @export
store_new.aws_feather <- function(class, file = NULL, resources = NULL) {
  aws_feather_new(file = file, resources = resources)
}

aws_feather_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c(
      "tar_aws_feather",
      "tar_aws",
      "tar_cloud",
      "tar_external",
      "tar_feather",
      "tar_store"
    )
  )
}

#' @export
store_assert_format_setting.aws_feather <- function(class) {
}
