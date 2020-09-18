url_exists <- function(url, handle = NULL) {
  unlist(lapply(url, url_exists_impl, handle = handle))
}

url_exists_impl <- function(url, handle) {
  assert_internet()
  handle <- url_handle(handle)
  req <- curl::curl_fetch_memory(as.character(url), handle = handle)
  identical(as.integer(req$status_code), 200L)
}

url_hash <- function(url, handle = NULL) {
  digest_obj64(lapply(url, url_hash_impl, handle = handle))
}

url_hash_impl <- function(url, handle) {
  assert_internet()
  handle <- url_handle(handle)
  req <- curl::curl_fetch_memory(url, handle = handle)
  assert_identical(req$status_code, 200L, paste("could not access url:", url))
  headers <- curl::parse_headers_list(req$headers)
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  out <- paste0(etag, mtime)
  assert_nzchar(out, paste("no ETag or Last-Modified for url:", url))
  out
}

url_handle <- function(handle = NULL) {
  handle <- handle %||% curl::new_handle()
  assert_inherits(handle, "curl_handle")
  curl::handle_setopt(handle, nobody = TRUE)
}
