resources_future_init <- function(
  resources = list()
) {
  resources_future_new(
    resources = resources
  )
}

resources_future_new <- function(
  resources = NULL
) {
  force(resources)
  enclass(environment(), c("tar_resources_future", "tar_resources"))
}

#' @export
resources_validate.tar_resources_future <- function(resources) {
  assert_list(resources$resources)
  if (length(resources$resources)) {
    assert_nonempty(names(resources$resources))
    assert_nzchar(names(resources$resources))
    assert_unique(names(resources$resources))
  }
}

#' @export
print.tar_resources_future <- function(x, ...) {
  cat(
    "<tar_resources_future>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
