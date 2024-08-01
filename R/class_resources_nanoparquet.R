resources_nanoparquet_init <- function(
  compression = "snappy"
) {
  resources_nanoparquet_new(
    compression = compression
  )
}

resources_nanoparquet_new <- function(
  compression = NULL
) {
  force(compression)
  enclass(environment(), c("tar_resources_nanoparquet", "tar_resources"))
}

#' @export
resources_validate.tar_resources_nanoparquet <- function(resources) {
  tar_assert_scalar(resources$compression)
  tar_assert_chr(resources$compression)
  tar_assert_nzchar(resources$compression)
}

#' @export
print.tar_resources_nanoparquet <- function(x, ...) {
  cat(
    "<tar_resources_nanoparquet>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
