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
  # TODO: use altrep when solved in qs (#147).
  qs::qread(file = path, use_alt_rep = FALSE)
}

#' @export
store_write_path.tar_qs <- function(store, object, path) {
  preset <- store$resources$preset %|||% "high"
  assert_chr(preset)
  assert_scalar(preset)
  qs::qsave(x = object, file = path, preset = preset)
}

#' @export
store_get_packages.tar_qs <- function(store) {
  "qs"
}
