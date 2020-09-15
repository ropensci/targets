# Tested in tests/interent/test-url.R
# nocov start
#' @export
store_new.url <- function(class, file) {
  store_url_new(file)
}

store_url_new <- function(file = NULL) {
  force(file)
  enclass(environment(), c("tar_url", "tar_store"))
}

#' @export
store_assert_format_setting.url <- function(class) {
}

#' @export
store_read_path.tar_url <- function(store, path) {
  path
}

#' @export
store_write_object.tar_url <- function(store, object) {
}

#' @export
store_write_path.tar_url <- function(store, object, path) {
}

#' @export
store_produce_path.tar_url <- function(store, name, object) {
  object
}

#' @export
store_coerce_object.tar_url <- function(store, object) {
  as.character(object)
}

#' @export
store_assert_format.tar_url <- function(store, object) {
  if (!is.null(object) && !is.character(object)) {
    throw_validate(
      "targets with format = \"url\" must return ",
      "character vectors of URL paths."
    )
  }
}

#' @export
store_early_hash.tar_url <- function(store) { # nolint
  store$file$hash <- store_url_hash(store$file$path)
}

store_url_hash <- function(urls) {
  digest_obj64(lapply(urls, store_url_hash_raw))
}

store_url_hash_raw <- function(url) {
  assert_internet()
  handle <- curl::new_handle()
  handle <- curl::handle_setopt(handle, nobody = TRUE)
  req <- curl::curl_fetch_memory(url, handle = handle)
  stopifnot(length(req$content) < 1L)
  headers <- curl::parse_headers_list(req$headers)
  assert_identical(req$status_code, 200L, paste("could not access url:", url))
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  out <- paste0(etag, mtime)
  assert_nzchar(out, paste("no ETag or Last-Modified for url:", url))
  out
}

#' @export
store_late_hash.tar_url <- function(store) { # nolint
}

#' @export
store_wait_correct_hash.tar_url <- function(store, remote) { # nolint
}

#' @export
store_validate_packages.tar_qs <- function(store) {
  assert_package("curl")
}

#' @export
store_warn_output.tar_url <- function(store, name) {
}
# nocov end
