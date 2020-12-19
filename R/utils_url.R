#' @title Random TCP port
#' @export
#' @keywords internal
#' @description Not a user-side function. Exported for infrastructure
#'   purposes only.
#' @return A random port not likely to be used by another process.
#' @param lower Integer of length 1, lowest possible port.
#' @param upper Integer of length 1, highest possible port.
#' @examples
#' tar_random_port()
tar_random_port <- function(lower = 49152L, upper = 65355L) {
  sample(seq.int(from = lower, to = upper, by = 1L), size = 1L)
}

url_exists <- function(url, handle = NULL) {
  unlist(lapply(url, url_exists_impl, handle = handle))
}

url_exists_impl <- function(url, handle) {
  assert_internet()
  handle <- url_handle(handle)
  tryCatch(url_exists_try(url, handle), error = function(e) FALSE)
}

url_exists_try <- function(url, handle) {
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

# Tested in tests/interactive/test-tar_watch.R. # nolint
# nocov start
url_port <- function(host, port, process, verbose = TRUE) {
  spin <- cli::make_spinner()
  trn(verbose, spin$spin(), NULL)
  while (!pingr::is_up(destination = host, port = port)) {
    trn(
      process$is_alive(),
      Sys.sleep(0.01),
      throw_run(process$read_all_error())
    )
    trn(verbose, spin$spin(), NULL)
  }
  trn(verbose, spin$finish(), NULL)
  url <- paste0("http://", host, ":", port)
  utils::browseURL(url)
}
# nocov end
