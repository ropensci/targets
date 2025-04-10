#' @export
store_row_path.tar_repository_cas_file <- store_row_path.tar_external

#' @export
store_hash_early.tar_repository_cas_file <- function(store, file) {
  store_hash_early.tar_store_file(store, file)
}

#' @export
store_hash_late.tar_repository_cas_file <- function(store, file) {
  store_hash_late.tar_store_file(store, file)
}

#' @export
store_upload_object.tar_repository_cas_file <- function(store, file) {
  store_upload_object_cas(store, file, file$path)
}

#' @export
store_read_object.tar_repository_cas_file <- function(store, file) {
  scratch <- path_scratch_temp_network()
  dir_create(dirname(scratch))
  on.exit(unlink(scratch))
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$download,
    args = list(path = scratch, key = file$hash)
  )
  dir_create(dirname(file$hash))
  file_move(from = scratch, to = file$path)
  file$path
}
