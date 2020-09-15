fst_dt_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_fst_dt", "tar_fst", "tar_store"))
}

#' @export
store_assert_format.fst_dt <- function(class) {
}

#' @export
store_read_path.tar_fst_dt <- function(store, path) {
  fst::read_fst(path, as.data.table = TRUE)
}

#' @export
store_coerce_object.tar_fst_dt <- function(store, object) {
  data.table::as.data.table(object)
}

#' @export
store_validate_packages.tar_fst_dt <- function(store) {
  assert_package("data.table")
  assert_package("fst")
}
