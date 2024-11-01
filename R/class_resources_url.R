resources_url_init <- function(
  handle = NULL,
  max_tries = 5,
  seconds_interval = 1,
  seconds_timeout = 60
) {
  resources_url_new(
    handle = handle,
    max_tries = max_tries,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
}

resources_url_new <- function(
  handle = NULL,
  max_tries = NULL,
  seconds_interval = NULL,
  seconds_timeout = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$handle <- handle
  out$max_tries <- max_tries
  out$seconds_interval <- seconds_interval
  out$seconds_timeout <- seconds_timeout
  enclass(out, resources_url_s3_class)
}

resources_url_s3_class <- c("tar_resources_url", "tar_resources")

#' @export
resources_validate.tar_resources_url <- function(resources) {
  if (!is.null(resources$handle)) {
    tar_assert_inherits(resources$handle, "curl_handle")
  }
  tar_assert_dbl(resources$max_tries)
  tar_assert_scalar(resources$max_tries)
  tar_assert_finite(resources$max_tries)
  tar_assert_ge(resources$max_tries, 0)
  tar_assert_dbl(resources$seconds_interval)
  tar_assert_scalar(resources$seconds_interval)
  tar_assert_finite(resources$seconds_interval)
  tar_assert_ge(resources$seconds_interval, 0)
  tar_assert_dbl(resources$seconds_timeout)
  tar_assert_scalar(resources$seconds_timeout)
  tar_assert_finite(resources$seconds_timeout)
  tar_assert_ge(resources$seconds_timeout, 0)
}

#' @export
print.tar_resources_url <- function(x, ...) {
  cat(
    "<tar_resources_url>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
