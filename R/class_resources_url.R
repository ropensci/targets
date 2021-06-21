resources_url_init <- function(
  handle = NULL
) {
  resources_url_new(
    handle = handle
  )
}

resources_url_new <- function(
  handle = NULL
) {
  force(handle)
  enclass(environment(), c("tar_resources_url", "tar_resources"))
}

#' @export
resources_validate.tar_resources_url <- function(resources) {
  if (!is.null(resources$handle)) {
    tar_assert_inherits(resources$handle, "curl_handle")
  }
}

#' @export
print.tar_resources_url <- function(x, ...) {
  cat(
    "<tar_resources_url>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
