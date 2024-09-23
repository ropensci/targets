#' @export
store_row_path.tar_external <- function(store) {
  store$file$path
}

#' @export
store_path_from_record.tar_external <- function(store, record, path_store) {
  record$path
}

#' @export
store_cache_path.tar_external <- function(store, path) {
}
