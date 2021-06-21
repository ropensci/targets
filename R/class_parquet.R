#' @export
store_new.parquet <- function(class, file = NULL, resources = NULL) {
  parquet_new(file, resources)
}

parquet_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_parquet", "tar_store"))
}

#' @export
store_assert_format_setting.parquet <- function(class) {
}

#' @export
store_read_path.tar_parquet <- function(store, path) {
  arrow::read_parquet(path)
}

#' @export
store_write_path.tar_parquet <- function(store, object, path) {
  args <- list(x = object, sink = path, version = "2.0")
  args$compression <- store$resources$feather$compression %|||%
    store$resources$compression
  args$compression_level <- store$resources$feather$compression_level %|||%
    store$resources$compression_level
  do.call(arrow::write_parquet, args)
}

#' @export
store_cast_object.tar_parquet <- function(store, object) {
  object
}

#' @export
store_assert_format.tar_parquet <- function(store, object, name) { # nolint
  msg <- paste0(
    "target ", name, " did not return a data frame. ",
    "Target with format = \"parquet\" must return data frames."
  )
  tar_assert_df(object, msg)
}

#' @export
store_get_packages.tar_parquet <- function(store) {
  "arrow"
}
