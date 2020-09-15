fst_tbl_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_fst_tbl", "tar_fst", "tar_store"))
}

#' @export
store_assert_format.fst_tbl <- function(format) {
}

#' @export
store_read_path.tar_fst_tbl <- function(store, path) {
  tibble::as_tibble(fst::read_fst(path))
}

#' @export
store_coerce_object.tar_fst_tbl <- function(store, object) {
  tibble::as_tibble(object)
}

#' @export
store_validate_packages.tar_fst_tbl <- function(store) {
  assert_package("fst")
  assert_package("tibble")
}
