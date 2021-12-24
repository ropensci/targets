#' @export
store_new.aws_keras <- function(format, file = NULL, resources = NULL) {
  aws_keras_new(file = file, resources = resources)
}

aws_keras_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c(
      "tar_aws_keras",
      "tar_aws",
      "tar_cloud",
      "tar_external",
      "tar_keras",
      "tar_nonexportable",
      "tar_store"
    )
  )
}

#' @export
store_assert_format_setting.aws_keras <- function(format) {
}
