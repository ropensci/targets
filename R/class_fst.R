#' @export
store_new.fst <- function(class, file = NULL, resources = NULL) {
  fst_new(file, resources)
}

fst_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_fst", "tar_store"))
}

#' @export
store_assert_format_setting.fst <- function(class) {
}

#' @export
store_read_path.tar_fst <- function(store, path) {
  fst::read_fst(path)
}

#' @export
store_write_path.tar_fst <- function(store, object, path) {
  compress <- store$resources$compress %|||% 50
  assert_dbl(compress)
  assert_scalar(compress)
  fst::write_fst(x = object, path = path, compress = compress)
}

#' @export
store_cast_object.tar_fst <- function(store, object) {
  as.data.frame(object)
}

#' @export
store_get_packages.tar_fst <- function(store) {
  "fst"
}
