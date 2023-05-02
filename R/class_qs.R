#' @export
store_class_format.qs <- function(format) {
  c("tar_qs", "tar_store")
}

#' @export
store_assert_format_setting.qs <- function(format) {
}

#' @export
store_read_path.tar_qs <- function(store, path) {
  qs::qread(file = path, use_alt_rep = TRUE)
}

#' @export
store_write_path.tar_qs <- function(store, object, path) {
  preset <- store$resources$qs$preset %|||%
    store$resources$preset %|||%
    "high"
  qs::qsave(x = object, file = path, preset = preset)
}

#' @export
store_get_packages.tar_qs <- function(store) {
  "qs"
}
