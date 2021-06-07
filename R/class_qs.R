#' @export
store_new.qs <- function(class, file = NULL, resources = NULL) {
  qs_new(file, resources)
}

qs_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_qs", "tar_store"))
}

#' @export
store_assert_format_setting.qs <- function(class) {
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
