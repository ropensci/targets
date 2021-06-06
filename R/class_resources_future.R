resources_future_init <- function(...) {
  resources_future_new(...)
}

resources_future_new <- function(...) {
  enclass(as.environment(list(...)), "tar_resources_future")
}

#' @export
resources_validate.tar_resources_future <- function(resources) {
}

#' @export
print.tar_resources_future <- function(x, ...) {
  cat(
    "<tar_resources_future>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
