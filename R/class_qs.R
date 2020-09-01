qs_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_qs", "tar_store"))
}

#' @export
store_read_path.tar_qs <- function(store, path) {
  qs::qread(file = path, use_alt_rep = TRUE)
}

#' @export
store_write_path.tar_qs <- function(store, object, path) {
  qs::qsave(x = object, file = path, preset = "high")
}

#' @export
store_validate_packages.tar_qs <- function(store) {
  assert_package("qs")
}
