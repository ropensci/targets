#' @export
store_class_format.nanoparquet <- function(format) {
  c("tar_nanoparquet", "tar_store")
}

#' @export
store_assert_format_setting.nanoparquet <- function(format) {
}

#' @export
store_read_path.tar_nanoparquet <- function(store, path) {
  object <- getNamespace("nanoparquet")[["read_parquet"]](
    file = path,
    options = store_nanoparquet_options()
  )
  metadata <- getNamespace("nanoparquet")[["parquet_metadata"]](file = path)
  classes <- metadata$file_meta_data$key_value_metadata[[1L]]
  class(object) <- classes$value[classes$key == "class"]
  object
}

#' @export
store_write_path.tar_nanoparquet <- function(store, object, path) {
  metadata <- class(object)
  names(metadata) <- rep("class", length(metadata))
  getNamespace("nanoparquet")[["write_parquet"]](
    x = object,
    file = path,
    metadata = metadata,
    compression = store$resources$nanoparquet[["compression"]],
    options = store_nanoparquet_options()
  )
}

#' @export
store_assert_format.tar_nanoparquet <- function(store, object, name) { # nolint
  msg <- paste(
    "target",
    name,
    "has nanoparquet format, so it must be a data frame",
    "which can be converted to a tibble."
  )
  tar_assert_inherits(
    x = object %|||% data.frame(),
    class = "data.frame",
    msg = msg
  )
}

#' @export
store_convert_object.tar_nanoparquet <- function(store, object) {
  if_any(is.null(object), as.data.frame(NULL), object)
}

#' @export
store_get_packages.tar_nanoparquet <- function(store) {
  "nanoparquet"
}

store_nanoparquet_options <- function() {
  getNamespace("nanoparquet")[["parquet_options"]](
    class = character(0L),
    use_arrow_metadata = TRUE,
    write_arrow_metadata = TRUE
  )
}
