#' @export
store_class_format.fst_tbl <- function(format) {
  c("tar_fst_tbl", "tar_fst", "tar_store")
}

#' @export
store_assert_format_setting.fst_tbl <- function(format) {
}

#' @export
store_read_path.tar_fst_tbl <- function(store, path) {
  tibble::as_tibble(fst::read_fst(path))
}

#' @export
store_convert_object.tar_fst_tbl <- function(store, object) {
  tibble::as_tibble(as.data.frame(object))
}

#' @export
store_get_packages.tar_fst_tbl <- function(store) {
  c("tibble", NextMethod())
}
