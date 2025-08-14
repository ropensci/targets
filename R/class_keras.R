#' @export
store_class_format.keras <- function(format) {
  store_class_format_keras
}

store_class_format_keras <- c("tar_keras", "tar_nonexportable", "tar_store")

#' @export
store_assert_format_setting.keras <- function(format) {}

# It would be too burdensome to depend on Python Keras
# just for automated testing, so this code is covered
# in semi-automated tests in tests/hpc/ and # nolint
# tests/interactive/. # nolint
# nocov start
#' @export
store_read_path.tar_keras <- function(store, path) {
  keras::load_model_hdf5(path)
}

#' @export
store_write_path.tar_keras <- function(store, object, path) {
  keras::save_model_hdf5(object = object, filepath = path)
}

#' @export
store_marshal_object.tar_keras <- function(store, object) {
  keras::serialize_model(object)
}

#' @export
store_unmarshal_object.tar_keras <- function(store, object) {
  keras::unserialize_model(object)
}
# nocov end

#' @export
store_get_packages.tar_keras <- function(store) {
  "keras"
}
