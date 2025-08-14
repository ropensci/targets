#' @export
store_class_format.auto <- function(format) {
  store_class_format_auto
}

store_class_format_auto <- c("tar_auto", "tar_store")

#' @export
store_assert_format_setting.auto <- function(format) {}

#' @export
store_get_packages.tar_auto <- function(store) {
  "qs2"
}

#' @export
store_path_from_name.tar_auto <- function(
  store,
  format,
  name,
  path,
  path_store
) {
  if (identical(format, "file")) {
    store_path_from_name.tar_external(
      store = store,
      format = format,
      name = name,
      path = path,
      path_store = path_store
    )
  } else {
    store_path_from_name.default(
      store = store,
      format = format,
      name = name,
      path = path,
      path_store = path_store
    )
  }
}

store_reformat_auto <- function(target) {
  if (target$settings$format != "auto") {
    return()
  }
  object <- target$value$object
  format <- if_any(
    is.character(object) && all(file.exists(object)),
    "file",
    "qs"
  )
  target_reformat(target, format)
}

#' @export
store_read_path.tar_auto <- function(store, path) {
  path_qs <- path_objects(.subset2(tar_runtime, "store"), basename(path))
  if_any(
    identical(path, path_qs),
    store_read_path.tar_qs(store = store, path = path),
    store_read_path.tar_store_file(store = store, path = path)
  )
}
