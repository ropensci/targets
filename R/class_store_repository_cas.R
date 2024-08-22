#' @export
store_class_repository.repository_cas <- function(repository, store, format) {
  format <- gsub(pattern = "\\&.*$", replacement = "", x = format)
  c(
    sprintf("tar_repository_cas_%s", format),
    "tar_repository_cas",
    class(store)
  )
}

#' @export
store_assert_repository_setting.repository_cas <- function(repository) {
}

