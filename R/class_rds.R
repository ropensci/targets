#' @export
store_new.rds <- function(class, file) {
  rds_new(file)
}

rds_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_rds", "tar_store"))
}

#' @export
store_assert_format_setting.rds <- function(class) {
}

#' @export
store_read_path.tar_rds <- function(store, path) {
  readRDS(path)
}

#' @export
store_write_path.tar_rds <- function(store, object, path) {
  saveRDS(object = object, file = path, version = 3L)
}
