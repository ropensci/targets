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
  assert_list(resources$template, "template must be a named list.")
  if (length(resources$template)) {
    assert_nonempty(names(resources$template), "template must have names.")
    assert_nzchar(
      names(resources$template),
      "template names must be nonempty"
    )
    assert_unique(names(resources$template), "template names must be unique.")
  }
}

#' @export
print.tar_resources_clustermq <- function(x, ...) {
  cat(
    "<tar_resources_clustermq>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
