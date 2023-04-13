resources_url_init <- function(
  handle = NULL,
  seconds_interval = 1,
  seconds_timeout = 10
) {
  resources_url_new(
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
}

resources_url_new <- function(
  handle = NULL,
  seconds_interval = NULL,
  seconds_timeout = NULL
) {
  force(handle)
  force(seconds_interval)
  force(seconds_timeout)
  enclass(environment(), c("tar_resources_url", "tar_resources"))
}

#' @export
resources_validate.tar_resources_url <- function(resources) {
  if (!is.null(resources$handle)) {
    tar_assert_inherits(resources$handle, "curl_handle")
  }
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
