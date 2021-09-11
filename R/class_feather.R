#' @export
store_new.feather <- function(class, file = NULL, resources = NULL) {
  feather_new(file, resources)
}

feather_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_feather", "tar_store"))
}

#' @export
store_assert_format_setting.feather <- function(class) {
}

#' @export
store_read_path.tar_feather <- function(store, path) {
  arrow::read_feather(path)
}

#' @export
store_write_path.tar_feather <- function(store, object, path) {
  args <- list(x = object, sink = path, version = 2L)
  args$compression <- store$resources$feather$compression %|||%
    store$resources$compression
  args$compression_level <- store$resources$feather$compression_level %|||%
    store$resources$compression_level
  do.call(arrow::write_feather, args)
}

#' @export
store_cast_object.tar_feather <- function(store, object) {
  object
}

#' @export
store_assert_format.tar_feather <- function(store, object, name) { # nolint
  msg <- paste(
    "target", name, "has feather format, so it must have class",
    "data.frame, RecordBatch, or Table."
  )
  tar_assert_inherits(
    x = object,
    class = c("data.frame", "RecordBatch", "Table"),
    msg = msg
  )
}

#' @export
store_get_packages.tar_feather <- function(store) {
  "arrow"
}
