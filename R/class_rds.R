#' @export
store_new.rds <- function(class, file = NULL, resources = NULL) {
  rds_new(file, resources)
}

rds_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
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
