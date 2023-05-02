#' @export
store_class_format.feather <- function(format) {
  c("tar_feather", "tar_store")
}

#' @export
store_assert_format_setting.feather <- function(format) {
}

#' @export
store_read_path.tar_feather <- function(store, path) {
  arrow::read_feather(path)
}

#' @export
store_write_path.tar_feather <- function(store, object, path) {
  args <- list(x = object, sink = path)
  fields <- c("compression", "compression_level")
  for (field in fields) {
    args[[field]] <- store$resources$feather[[field]] %|||%
      store$resources[[field]]
  }
  args <- omit_null(args)
  do.call(arrow::write_feather, args)
}

#' @export
store_assert_format.tar_feather <- function(store, object, name) { # nolint
  msg <- paste(
    "target", name, "has feather format, so it must have class",
    "data.frame, RecordBatch, or Table."
  )
  tar_assert_inherits(
    x = object %|||% data.frame(),
    class = c("data.frame", "RecordBatch", "Table"),
    msg = msg
  )
}

#' @export
store_convert_object.tar_feather <- function(store, object) {
  if_any(
    is.null(object),
    as.data.frame(NULL),
    object
  )
}

#' @export
store_get_packages.tar_feather <- function(store) {
  "arrow"
}
