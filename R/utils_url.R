#' @title Random TCP port
#' @export
#' @keywords internal
#' @description Not a user-side function. Exported for infrastructure
#'   purposes only.
#' @return A random port not likely to be used by another process.
#' @param lower Integer of length 1, lowest possible port.
#' @param upper Integer of length 1, highest possible port.
#' @examples
#' if (requireNamespace("parallelly", quietly = TRUE)) {
#' tar_random_port()
#' }
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
  seconds_timeout,
  max_tries,
  verbose
) {
  envir <- new.env(parent = emptyenv())
  envir$out <- rep(FALSE, length(url))
  out <- map_lgl(
    url,
    url_exists_impl,
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
  all(out)
}

url_exists_impl <- function(
  url,
  handle,
  seconds_interval,
  seconds_timeout,
  max_tries,
  verbose
) {
  req <- url_request(
    url = url,
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
  url_status_success(req$status_code)
}

url_hash <- function(
  url,
  handle = NULL,
  seconds_interval,
  seconds_timeout,
  max_tries,
  verbose
) {
  envir <- new.env(parent = emptyenv())
  hash_object(
    lapply(
      url,
      url_hash_impl,
      handle = handle,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      max_tries = max_tries,
      verbose = verbose
    )
  )
}

url_hash_impl <- function(
  url,
  handle,
  seconds_interval,
  seconds_timeout,
  max_tries,
  verbose
) {
  req <- url_request(
    url = url,
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
  headers <- curl::parse_headers_list(req$headers)
  url_process_error(url = url, req = req, headers = headers)
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

url_request <- function(
  url,
  handle,
  seconds_interval,
  seconds_timeout,
  max_tries,
  verbose
) {
  tar_assert_internet()
  handle <- url_handle(handle)
  envir <- new.env(parent = emptyenv())
  retry_until_true(
    fun = ~ {
      envir$req <- curl::curl_fetch_memory(url, handle = handle)
      !(envir$req$status_code %in% http_retry_codes)
    },
    args = list(url = url, handle = handle),
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    message = paste("could not access URL:", url),
    max_tries = max_tries,
    catch_error = FALSE,
    verbose = verbose
  )
  envir$req
}

url_status_success <- function(status_code) {
  (status_code >= 200L) && (status_code < 300L)
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

http_retry_codes <- c(429L, seq(from = 500L, to = 599))
http_retry <- paste0("http_", http_retry_codes)
