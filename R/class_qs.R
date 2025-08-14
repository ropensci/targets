#' @export
store_class_format.qs <- function(format) {
  store_class_format_qs
}

store_class_format_qs <- c("tar_qs", "tar_store")

#' @export
store_assert_format_setting.qs <- function(format) {}

#' @export
store_read_path.tar_qs <- function(store, path) {
  tryCatch(
    qs2::qs_read(
      file = path,
      validate_checksum = FALSE,
      nthreads = store$resources$qs$nthreads %|||% 1L
    ),
    # nocov start
    error = function(condition) {
      getNamespace("qs")$qread(
        file = path,
        use_alt_rep = TRUE,
        nthreads = store$resources$qs$nthreads %|||% 1L
      )
    }
    # nocov end
  )
}

#' @export
store_write_path.tar_qs <- function(store, object, path) {
  qs2::qs_save(
    object = object,
    file = path,
    compress_level = store$resources$qs$compress_level %|||% 3L,
    shuffle = store$resources$qs$shuffle %|||% TRUE,
    nthreads = store$resources$qs$nthreads %|||% 1L
  )
}

#' @export
store_get_packages.tar_qs <- function(store) {
  "qs2"
}
