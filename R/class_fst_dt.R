#' @export
store_class_format.fst_dt <- function(format) {
  c("tar_fst_dt", "tar_fst", "tar_store")
}

#' @export
store_assert_format_setting.fst_dt <- function(format) {
}

#' @export
store_read_path.tar_fst_dt <- function(store, path) {
  fst::read_fst(path, as.data.table = TRUE)
}

#' @export
store_convert_object.tar_fst_dt <- function(store, object) {
  data.table::as.data.table(object)
}

#' @export
store_copy_object.tar_fst_dt <- function(store, object) {
  data.table::copy(object)
}

#' @export
store_get_packages.tar_fst_dt <- function(store) {
  c("data.table", NextMethod())
}
