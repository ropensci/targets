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
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$upload,
    args = list(key = store$file$hash, path = store$file$path)
  )
  tar_assert_true(
    all(file.exists(store$file$path)),
    msg = paste0(
      "CAS repository upload deleted file ",
      store$file$path,
      ". Uploads should not delete format = \"file\" output files."
    )
  )
}

#' @export
store_read_object.tar_repository_cas_file <- function(store) {
  scratch <- path_scratch_temp_network()
  dir_create(dirname(scratch))
  on.exit(unlink(scratch))
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$download,
    args = list(path = scratch, key = store$file$hash)
  )
  dir_create(dirname(store$file$hash))
  file_move(from = scratch, to = store$file$path)
  store$file$path
}
