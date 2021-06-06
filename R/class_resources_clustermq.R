resources_clustermq_init <- function(...) {
  resources_clustermq_new(...)
}

resources_clustermq_new <- function(...) {
  enclass(as.environment(list(...)), "tar_resources_clustermq")
}

#' @export
resources_validate.tar_resources_clustermq <- function(resources) {
}

#' @export
print.tar_resources_clustermq <- function(x, ...) {
  cat(
    "<tar_resources_clustermq>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
