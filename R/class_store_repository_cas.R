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
store_hash_early.tar_repository_cas <- function(store, file) {
}

#' @export
store_hash_late.tar_repository_cas <- function(store, file) {
  tar_assert_file(file$stage)
  path <- file$path
  on.exit(file$path <- path)
  file$path <- file$stage
  file_update_hash(file)
}

#' @export
store_upload_object.tar_repository_cas <- function(store, file) {
  store_upload_object_cas(store, file, file$stage)
}

store_upload_object_cas <- function(store, file, path) {
  on.exit(unlink(store$file$stage, recursive = TRUE, force = TRUE))
  tar_assert_scalar(
    path,
    msg = paste(
      "for a tar_repository_cas() target, the output must be",
      "a single file or single directory."
    )
  )
  key <- file$hash
  lookup <- tar_repository_cas_lookup(store)
  if (lookup_missing(lookup, key) || !lookup_get(lookup, key)) {
    store_repository_cas_call_method(
      store = store,
      text = store$methods_repository$upload,
      args = list(key = key, path = path)
    )
    lookup_unset(lookup = lookup, names = key)
  }
}

#' @export
store_read_object.tar_repository_cas <- function(store, file) {
  scratch <- path_scratch_temp_network()
  dir_create(dirname(scratch))
  on.exit(unlink(scratch))
  store_repository_cas_call_method(
    store = store,
    text = store$methods_repository$download,
    args = list(key = file$hash, path = scratch)
  )
  store_convert_object(store, store_read_path(store, scratch))
}

#' @export
store_has_correct_hash.tar_repository_cas <- function(store, file) {
  lookup <- tar_repository_cas_lookup(store)
  key <- .subset2(file, "hash")
  if (lookup_missing(lookup = lookup, name = key)) {
    object <- store_repository_cas_call_method(
      store = store,
      text = store$methods_repository$exists,
      args = list(key = key)
    )
    lookup_set(lookup = lookup, names = key, object = object)
  }
  lookup_get(lookup = lookup, name = key)
}

#' @export
store_ensure_correct_hash.tar_repository_cas <- function(
  store,
  file,
  storage,
  deployment
) {
  if (!store$methods_repository$consistent) {
    store_wait_correct_hash(store, file)
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

tar_repository_cas_lookup <- function(store) {
  meta <- .subset2(tar_runtime, "meta")
  lookup_table <- .subset2(meta, "repository_cas_lookup_table")
  methods <- .subset2(store, "methods_repository")
  repository <- .subset2(methods, "repository")
  lookup <- lookup_get(lookup_table, repository)
  if (is.environment(lookup)) {
    return(lookup)
  }
  list_method <- .subset2(methods, "list")
  if (all(list_method == "NULL")) {
    return(lookup_new())
  }
  keys_meta <- as.character(lookup)
  keys_cas <- store_repository_cas_call_method(
    store = store,
    text = list_method,
    args = list(keys = keys_meta)
  )
  lookup <- lookup_new()
  lookup_set(lookup, names = as.character(keys_cas), object = TRUE)
  lookup_set(lookup, names = setdiff(keys_meta, keys_cas), object = FALSE)
  lookup_set(lookup_table, names = repository, object = lookup)
  lookup
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
  what <- eval(parse(text = text, keep.source = FALSE), envir = envir)
  do.call(what = what, args = args, envir = envir)
}
