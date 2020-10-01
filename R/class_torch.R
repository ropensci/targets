#' @export
store_new.torch <- function(class, file = NULL, resources = NULL) {
  torch_new(file, resources)
}

torch_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_torch", "tar_store"))
}

#' @export
store_assert_format_setting.torch <- function(class) {
}

#' @export
store_read_path.tar_torch <- function(store, path) {
  torch::torch_load(path)
}

#' @export
store_write_path.tar_torch <- function(store, object, path) {
  torch::torch_save(obj = object, path = path)
}

#' @export
store_serialize_value.tar_torch <- function(store, value) {
  con <- rawConnection(raw(), open = "wr")
  on.exit(close(con))
  torch::torch_save(value$object, con)
  value$object <- rawConnectionValue(con)
}

#' @export
store_unserialize_value.tar_torch <- function(store, value) {
  con <- rawConnection(value$object, open = "r")
  on.exit(close(con))
  value$object <- torch::torch_load(con)
}

#' @export
store_validate_packages.tar_torch <- function(store) {
  assert_package("torch")
}
