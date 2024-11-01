#' @export
store_class_format.rds <- function(format) {
  store_class_format_rds
}

store_class_format_rds <- c("tar_rds", "tar_store")

#' @export
store_assert_format_setting.rds <- function(format) {
}

#' @export
store_read_path.tar_rds <- function(store, path) {
  readRDS(path)
}

#' @export
store_write_path.tar_rds <- function(store, object, path) {
  saveRDS(object = object, file = path, version = 3L)
}
