resources_clustermq_init <- function(
  template = list()
) {
  resources_clustermq_new(
    template = template
  )
}

resources_clustermq_new <- function(
  template = NULL
) {
  force(template)
  enclass(environment(), c("tar_resources_clustermq", "tar_resources"))
}

#' @export
resources_validate.tar_resources_clustermq <- function(resources) {
  tar_assert_list(resources$template)
  if (length(resources$template)) {
    tar_assert_nonempty(names(resources$template))
    tar_assert_nzchar(names(resources$template))
    tar_assert_unique(names(resources$template))
  }
}

#' @export
print.tar_resources_clustermq <- function(x, ...) {
  cat(
    "<tar_resources_clustermq>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
