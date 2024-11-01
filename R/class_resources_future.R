resources_future_init <- function(
  plan = NULL,
  resources = list()
) {
  resources_future_new(
    plan = plan,
    resources = resources
  )
}

resources_future_new <- function(
  plan = NULL,
  resources = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$plan <- plan
  out$resources <- resources
  enclass(out, resources_future_s3_class)
}

resources_future_s3_class <- c("tar_resources_future", "tar_resources")

#' @export
resources_validate.tar_resources_future <- function(resources) {
  tar_assert_list(resources$resources)
  if (length(resources$resources)) {
    tar_assert_nonempty(names(resources$resources))
    tar_assert_nzchar(names(resources$resources))
    tar_assert_unique(names(resources$resources))
  }
}

#' @export
print.tar_resources_future <- function(x, ...) {
  cat(
    "<tar_resources_future>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
