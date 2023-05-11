resources_network_init <- function(
  seconds_interval = 1,
  seconds_timeout = 10,
  max_tries = 1L,
  verbose = TRUE
) {
  resources_network_new(
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
  )
}

resources_network_new <- function(
  seconds_interval = NULL,
  seconds_timeout = NULL,
  max_tries = NULL,
  verbose = NULL
) {
  force(seconds_interval)
  force(seconds_timeout)
  force(max_tries)
  force(verbose)
  enclass(environment(), c("tar_resources_network", "tar_resources"))
}

#' @export
resources_validate.tar_resources_network <- function(resources) {
  fields <- c("seconds_interval", "seconds_timeout", "max_tries")
  for (field in fields) {
    tar_assert_dbl(resources[[field]])
    tar_assert_scalar(resources[[field]])
    tar_assert_none_na(resources[[field]])
    tar_assert_ge(resources[[field]], 0)
  }
  tar_assert_scalar(resources$verbose)
  tar_assert_lgl(resources$verbose)
  tar_assert_none_na(resources$verbose)
}

#' @export
print.tar_resources_network <- function(x, ...) {
  cat(
    "<tar_resources_network>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
