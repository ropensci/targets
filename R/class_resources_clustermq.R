resources_clustermq_init <- function(
  log_worker = FALSE,
  template = list()
) {
  resources_clustermq_new(
    log_worker = log_worker,
    template = template
  )
}

resources_clustermq_new <- function(
  log_worker = NULL,
  template = NULL
) {
  force(log_worker)
  force(template)
  enclass(environment(), c("tar_resources_clustermq", "tar_resources"))
}

#' @export
resources_validate.tar_resources_clustermq <- function(resources) {
  assert_lgl(resources$log_worker, "log_worker must be logical.")
  assert_scalar(resources$log_worker, "log_worker must have length 1.")
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
