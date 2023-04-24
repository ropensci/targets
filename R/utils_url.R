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
  tar_assert_package("parallelly")
  ports <- seq.int(from = lower, to = upper, by = 1L)
  parallelly::freePort(ports = ports, default = NA_integer_)
}

# Tested in tests/interactive/test-class_url.R,
# not in testthat due to unreliable URLs.
# nocov start
url_exists <- function(
  url,
  handle = NULL,
  seconds_interval,
  seconds_timeout
) {
  tar_assert_internet()
  handle <- url_handle(handle)
  envir <- new.env(parent = emptyenv())
  envir$out <- rep(FALSE, length(url))
  retry(
    ~{
      envir$out <- map_lgl(url, url_exists_impl, handle = handle)
      all(envir$out)
    },
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    catch_error = TRUE,
    message = paste("Cannot connect to url:", url)
  )
  envir$out
}

url_exists_impl <- function(url, handle) {
  tryCatch(url_exists_try(url, handle), error = function(e) FALSE)
}

url_exists_try <- function(url, handle) {
  req <- curl::curl_fetch_memory(as.character(url), handle = handle)
  url_status_success(req$status_code)
}

url_hash <- function(url, handle = NULL, seconds_interval, seconds_timeout) {
  envir <- new.env(parent = emptyenv())
  retry(
    ~{
      envir$out <- digest_obj64(lapply(url, url_hash_impl, handle = handle))
      TRUE
    },
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    catch_error = TRUE,
    message = paste("Cannot connect to url:", url)
  )
  envir$out
}

url_hash_impl <- function(url, handle) {
  headers <- url_headers(url = url, handle = handle)
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  out <- paste0(etag, mtime)
  tar_assert_nzchar(
    out,
    paste0(
      "no ETag or Last-Modified for url: ",
      url,
      ". Please choose a storage format other than format = \"url\" ",
      "and pursue different workarounds to rerun the target ",
      "when the remote data changes."
    )
  )
  out
}

url_handle <- function(handle = NULL) {
  handle <- handle %|||% curl::new_handle(nobody = TRUE)
  tar_assert_inherits(handle, "curl_handle")
  handle
}

url_headers <- function(url, handle) {
  tar_assert_internet()
  handle <- url_handle(handle)
  req <- curl::curl_fetch_memory(url, handle = handle)
  headers <- curl::parse_headers_list(req$headers)
  url_process_error(url, req, headers)
  headers
}

url_process_error <- function(url, req, headers) {
  if (url_status_success(req$status_code)) {
    return()
  }
  header_text <- paste0("  ", names(headers), " = ", headers)
  header_text <- paste0(header_text, collapse = "\n")
  msg <- paste0(
    "HTTP response status code ",
    req$status_code,
    "\nCould not access url:\n  ",
    url,
    "\nHTTP response headers:\n",
    header_text
  )
  tar_throw_run(msg)
}

url_status_success <- function(status_code) {
  (status_code >= 200L) && (status_code < 300L)
}
# nocov end

# Tested in tests/interactive/test-tar_watch.R. # nolint
# nocov start
url_port <- function(host, port, process, verbose = TRUE) {
  spin <- cli::make_spinner()
  if_any(verbose, spin$spin(), NULL)
  while (!pingr::is_up(destination = host, port = port)) {
    if_any(
      process$is_alive(),
      Sys.sleep(0.01),
      tar_throw_run(process$read_all_error())
    )
    if_any(verbose, spin$spin(), NULL)
  }
  if_any(verbose, spin$finish(), NULL)
  url <- paste0("http://", host, ":", port)
  utils::browseURL(url)
}
# nocov end
