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
  force(compression)
  force(compression_level)
  enclass(environment(), c("tar_resources_parquet", "tar_resources"))
}

#' @export
resources_validate.tar_resources_parquet <- function(resources) {
  assert_scalar(resources$compression)
  assert_chr(resources$compression)
  assert_nzchar(resources$compression)
  assert_scalar(resources$compression_level %|||% 1)
  assert_dbl(resources$compression_level %|||% 1)
}

#' @export
print.tar_resources_parquet <- function(x, ...) {
  cat(
    "<tar_resources_parquet>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
