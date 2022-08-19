#' @export
store_new.parquet <- function(format, file = NULL, resources = NULL) {
  parquet_new(file, resources)
}

parquet_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_parquet", "tar_store"))
}

#' @export
store_assert_format_setting.parquet <- function(format) {
}

#' @export
store_read_path.tar_parquet <- function(store, path) {
  arrow::read_parquet(path)
}

#' @export
store_write_path.tar_parquet <- function(store, object, path) {
  args <- list(x = object, sink = path)
  fields <- c("compression", "compression_level")
  for (field in fields) {
    args[[field]] <- store$resources$parquet[[field]] %|||%
      store$resources[[field]]
  }
  args <- omit_null(args)
  do.call(arrow::write_parquet, args)
}

#' @export
store_assert_format.tar_parquet <- function(store, object, name) { # nolint
  msg <- paste(
    "target", name, "has parquet format, so it must have class",
    "data.frame, RecordBatch, or Table."
  )
  tar_assert_inherits(
    x = object,
    class = c("data.frame", "RecordBatch", "Table"),
    msg = msg
  )
}

#' @export
store_get_packages.tar_parquet <- function(store) {
  "arrow"
}
