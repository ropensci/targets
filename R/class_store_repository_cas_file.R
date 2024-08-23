#' @export
store_hash_early.tar_repository_cas_file <- function(store) {
  store_hash_early.tar_store_file(store)
}

#' @export
store_hash_late.tar_repository_cas_file <- function(store) {
  store_hash_late.tar_store_file(store)
}

#' @export
store_upload_object.tar_repository_cas_file <- function(store) {
  tar_assert_scalar(
    store$file$path,
    msg = paste(
      "for a\ tar_repository_cas() target, the output must be",
      "a single file or single directory."
    )
  )
  store_format_custom_call_method(
    store = store,
    text = store$methods_repository$upload,
    args = list(path = store$file$path, key = store$file$hash)
  )
}

#' @export
store_ensure_correct_hash.tar_repository_cas <- function(
  store,
  storage,
  deployment
) {
  if (!store$methods_repository$consistent) {
    store_ensure_correct_hash.tar_store_file(
      store = store,
      storage = storage,
      deployment = deployment
    )
  }
}

#' @export
store_read_object.tar_repository_cas_file <- function(store) {
  scratch <- path_scratch_temp_network()
  store_format_custom_call_method(
    store = store,
    text = store$methods_repository$download,
    args = list(path = scratch, key = store$file$hash)
  )
  dir_create(dirname(store$file$hash))
  file_move(from = scratch, to = store$file$path)
  store$file$path
}
