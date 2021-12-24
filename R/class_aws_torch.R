#' @export
store_new.aws_torch <- function(format, file = NULL, resources = NULL) {
  aws_torch_new(file = file, resources = resources)
}

aws_torch_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c(
      "tar_aws_torch",
      "tar_aws",
      "tar_cloud",
      "tar_external",
      "tar_torch",
      "tar_nonexportable",
      "tar_store"
    )
  )
}

#' @export
store_assert_format_setting.aws_torch <- function(format) {
}
