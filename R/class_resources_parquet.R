resources_parquet_init <- function(
  compression = "snappy",
  compression_level = NULL
) {
  resources_parquet_new(
    compression = compression,
    compression_level = compression_level
  )
}

resources_parquet_new <- function(
  compression = NULL,
  compression_level = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$compression <- compression
  out$compression_level <- compression_level
  enclass(out, resources_parquet_s3_class)
}

resources_parquet_s3_class <- c("tar_resources_parquet", "tar_resources")

#' @export
resources_validate.tar_resources_parquet <- function(resources) {
  tar_assert_scalar(resources$compression)
  tar_assert_chr(resources$compression)
  tar_assert_nzchar(resources$compression)
  tar_assert_scalar(resources$compression_level %|||% 1)
  tar_assert_dbl(resources$compression_level %|||% 1)
}

#' @export
print.tar_resources_parquet <- function(x, ...) {
  cat(
    "<tar_resources_parquet>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
