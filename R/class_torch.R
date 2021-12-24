#' @export
store_new.torch <- function(format, file = NULL, resources = NULL) {
  torch_new(file, resources)
}

torch_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_torch", "tar_nonexportable", "tar_store"))
}

#' @export
store_assert_format_setting.torch <- function(format) {
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
store_marshal_object.tar_torch <- function(store, object) {
  con <- rawConnection(raw(), open = "wr")
  on.exit(close(con))
  torch::torch_save(object, con)
  rawConnectionValue(con)
}

#' @export
store_unmarshal_object.tar_torch <- function(store, object) {
  con <- rawConnection(object, open = "r")
  on.exit(close(con))
  torch::torch_load(con)
}

#' @export
store_get_packages.tar_torch <- function(store) {
  "torch"
}
