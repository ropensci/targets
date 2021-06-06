resources_url_init <- function(
  handle = character(0)
) {
  resources_url_new(
    handle = handle
  )
}

resources_url_new <- function(
  handle = NULL
) {
  force(handle)
  enclass(environment(), "tar_resources_url")
}

#' @export
resources_validate.tar_resources_url <- function(resources) {
  assert_inherits(resources$handle, "curl_handle")
}

#' @export
print.tar_resources_url <- function(x, ...) {
  cat("<url>\n ", paste0(paste_list(as.list(x)), collapse = "\n  "))
}
