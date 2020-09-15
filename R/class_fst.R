fst_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_fst", "tar_store"))
}

#' @export
store_assert_format.fst <- function(class) {
}

#' @export
store_read_path.tar_fst <- function(store, path) {
  fst::read_fst(path)
}

#' @export
store_write_path.tar_fst <- function(store, object, path) {
  fst::write_fst(x = object, path = path)
}

#' @export
store_coerce_object.tar_fst <- function(store, object) {
  as.data.frame(object)
}

#' @export
store_validate_packages.tar_fst <- function(store) {
  assert_package("fst")
}
