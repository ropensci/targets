#' @export
store_class_format.fst <- function(format) {
  c("tar_fst", "tar_store")
}

#' @export
store_assert_format_setting.fst <- function(format) {
}

#' @export
store_read_path.tar_fst <- function(store, path) {
  fst::read_fst(path)
}

#' @export
store_write_path.tar_fst <- function(store, object, path) {
  compress <- store$resources$fst$compress %|||%
    store$resources$compress %|||%
    50
  tar_assert_dbl(compress)
  tar_assert_scalar(compress)
  fst::write_fst(x = object, path = path, compress = compress)
}

#' @export
store_convert_object.tar_fst <- function(store, object) {
  as.data.frame(object)
}

#' @export
store_get_packages.tar_fst <- function(store) {
  "fst"
}
