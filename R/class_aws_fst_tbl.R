#' @export
store_new.aws_fst_tbl <- function(class, file = NULL, resources = NULL) {
  aws_fst_tbl_new(file = file, resources = resources)
}

aws_fst_tbl_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c(
      "tar_aws_fst_tbl",
      "tar_aws",
      "tar_external",
      "tar_fst_tbl",
      "tar_fst",
      "tar_store"
    )
  )
}

#' @export
store_assert_format_setting.aws_fst_tbl <- function(class) {
}
