#' @export
store_class_format.file_fast <- function(format) {
  c("tar_store_file_fast", "tar_store_file", "tar_external", "tar_store")
}

#' @export
store_assert_format_setting.file_fast <- function(format) {
}

#' @export
store_set_timestamp_trust.tar_store_file_fast <- function(store) {
  store$file$trust_timestamps <- TRUE
}
