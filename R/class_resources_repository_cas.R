resources_repository_cas_init <- function(
  envvars = NULL
) {
  resources_repository_cas_new(
    envvars = envvars
  )
}

resources_repository_cas_new <- function(
  envvars = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$envvars <- envvars
  enclass(out, resources_repository_cas_s3_class)
}

resources_repository_cas_s3_class <- c(
  "tar_resources_repository_cas",
  "tar_resources"
)

#' @export
resources_validate.tar_resources_repository_cas <- function(resources) {
  if (!is.null(resources$envvars)) {
    tar_assert_chr(resources$envvars)
    tar_assert_none_na(resources$envvars)
    tar_assert_named(resources$controller)
  }
}

#' @export
print.tar_resources_repository_cas <- function(x, ...) {
  cat(
    "<tar_resources_repository_cas>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
