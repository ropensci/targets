store_repository_cas_methods_init <- function(repository) {
  repository <- unlist(strsplit(repository, split = "&", fixed = TRUE))
  store_repository_cas_methods_new(
    upload = store_repository_cas_field(repository, pattern = "^upload="),
    download = store_repository_cas_field(repository, pattern = "^download="),
    exists = store_repository_cas_field(repository, pattern = "^exists=")
  )
}

store_repository_cas_methods_new <- function(
  upload = NULL,
  download = NULL,
  exists = NULL
) {
  force(upload)
  force(download)
  force(exists)
  environment()
}

store_repository_cas_methods_validate <- function(methods) {
  tar_assert_correct_fields(methods, store_repository_cas_methods_new)
  for (field in c("upload", "download", "exists")) {
    tar_assert_chr(methods[[field]])
    tar_assert_scalar(methods[[field]])
    tar_assert_nzchar(methods[[field]])
  }
}

store_repository_cas_field <- function(repository, pattern) {
  base64url::base64_urldecode(keyvalue_field(repository, pattern))
}
