#' @export
store_class_repository.repository_cas <- function(repository, store, format) {
  format <- gsub(pattern = "\\&.*$", replacement = "", x = format)
  c(
    sprintf("tar_repository_cas_%s", format),
    "tar_repository_cas",
    "tar_cloud",
    if_any("tar_external" %in% class(store), character(0), "tar_external"),
    class(store)
  )
}

#' @export
store_assert_repository_setting.repository_cas <- function(repository) {
}

#' @export
store_hash_early.tar_repository_cas <- function(store) {
}

#' @export
store_hash_late.tar_repository_cas <- function(store) {
  tar_assert_file(store$file$stage)
  path <- store$file$path
  on.exit(store$file$path <- path)
  store$file$path <- store$file$stage
  file_update_hash(store$file)
}

#' @export
store_upload_object.tar_repository_cas <- function(store) {
  on.exit(unlink(store$file$stage, recursive = TRUE, force = TRUE))
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$upload,
    args = list(key = store$file$hash, path = store$file$stage)
  )
  tar_assert_true(
    all(file.exists(store$file$stage)),
    msg = paste0(
      "CAS repository upload attempted deleted file ",
      store$file$stage,
      ". Uploads should not delete the output files from targets ",
      "because it causes problems with format = \"file\"."
    )
  )
}

#' @export
store_read_object.tar_repository_cas <- function(store) {
  scratch <- path_scratch_temp_network()
  dir_create(dirname(scratch))
  on.exit(unlink(scratch))
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$download,
    args = list(key = store$file$hash, path = scratch)
  )
  store_convert_object(store, store_read_path(store, scratch))
}

#' @export
store_has_correct_hash.tar_repository_cas <- function(store) {
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$exists,
    args = list(key = store$file$hash)
  )
}

#' @export
store_ensure_correct_hash.tar_repository_cas <- function(
  store,
  storage,
  deployment
) {
  if (!store$methods_repository$consistent) {
    store_wait_correct_hash(store)
  }
}

#' @export
store_delete_objects.tar_repository_cas <- function(
  store,
  meta,
  batch_size,
  verbose
) {
}

store_repository_cas_call_method <- function(store, text, args) {
  envvars <- store$resources$repository_cas$envvars
  if (length(envvars)) {
    names <- names(envvars)
    previous <- Sys.getenv(names, names = TRUE)
    on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
    do.call(what = Sys.setenv, args = as.list(envvars))
  }
  envir <- new.env(parent = baseenv())
  what <- eval(parse(text = text), envir = envir)
  do.call(what = what, args = args, envir = envir)
}
