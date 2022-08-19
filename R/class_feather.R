#' @export
store_new.feather <- function(format, file = NULL, resources = NULL) {
  feather_new(file, resources)
}

feather_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_feather", "tar_store"))
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
    x = object,
    class = c("data.frame", "RecordBatch", "Table"),
    msg = msg
  )
}

#' @export
store_get_packages.tar_feather <- function(store) {
  "arrow"
}
