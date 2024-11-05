store_repository_cas_methods_init <- function(repository) {
  splits <- unlist(strsplit(repository, split = "&", fixed = TRUE))
  store_repository_cas_methods_new(
    repository = repository,
    upload = store_repository_cas_field(splits, pattern = "^upload="),
    download = store_repository_cas_field(splits, pattern = "^download="),
    exists = store_repository_cas_field(splits, pattern = "^exists="),
    list = store_repository_cas_field(splits, pattern = "^list="),
    consistent = as.logical(
      store_repository_cas_field(splits, pattern = "^consistent=")
    )
  )
}

store_repository_cas_methods_new <- function(
  repository = NULL,
  upload = NULL,
  download = NULL,
  exists = NULL,
  list = NULL,
  consistent = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$repository <- repository
  out$upload <- upload
  out$download <- download
  out$exists <- exists
  out$list <- list
  out$consistent <- consistent
  out
}

store_repository_cas_methods_validate <- function(methods) {
  tar_assert_chr(methods$repository)
  tar_assert_scalar(methods$repository)
  tar_assert_nzchar(methods$repository)
  tar_assert_correct_fields(methods, store_repository_cas_methods_new)
  for (field in c("upload", "download", "exists", "list")) {
    tar_assert_chr(methods[[field]])
    tar_assert_scalar(methods[[field]])
    tar_assert_nzchar(methods[[field]])
  }
  tar_assert_scalar(methods[["consistent"]])
  tar_assert_lgl(methods[["consistent"]])
  tar_assert_none_na(methods[["consistent"]])
}

store_repository_cas_field <- function(repository, pattern) {
  base64url::base64_urldecode(keyvalue_field(repository, pattern))
}
