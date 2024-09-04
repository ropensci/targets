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
  store_upload_object_cas(store, store$file$path)
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

#' @export
store_unload.tar_repository_cas_file <- function(store, target) {
  unlink(as.character(target$value$object))
  NextMethod()
}
