url_exists <- function(url) {
  unlist(lapply(url, url_exists_impl))
}

url_exists_impl <- function(url) {
  assert_internet()
  handle <- curl::new_handle(nobody = TRUE)
  req <- curl::curl_fetch_memory(as.character(url), handle = handle)
  identical(as.integer(req$status_code), 200L)
}

url_hash <- function(url) {
  digest_obj64(lapply(url, url_hash_impl))
}

url_hash_impl <- function(url) {
  assert_internet()
  handle <- curl::new_handle(nobody = TRUE)
  req <- curl::curl_fetch_memory(url, handle = handle)
  assert_identical(req$status_code, 200L, paste("could not access url:", url))
  headers <- curl::parse_headers_list(req$headers)
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  out <- paste0(etag, mtime)
  assert_nzchar(out, paste("no ETag or Last-Modified for url:", url))
  out
}
