#' @export
store_class_repository.gcp <- function(repository, store, format) {
  format <- gsub(pattern = "\\&.*$", replacement = "", x = format)
  c(
    sprintf("tar_gcp_%s", format),
    "tar_gcp",
    "tar_cloud",
    if_any("tar_external" %in% class(store), character(0), "tar_external"),
    class(store)
  )
}

#' @export
store_assert_repository_setting.gcp <- function(repository) {
}

#' @export
store_produce_path.tar_gcp <- function(store, name, object, path_store) {
  store_produce_gcp_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

store_produce_gcp_path <- function(store, name, object, path_store) {
  bucket <- store$resources$gcp$bucket %|||% store$resources$bucket
  tar_assert_nonempty(bucket)
  tar_assert_chr(bucket)
  tar_assert_scalar(bucket)
  tar_assert_nzchar(bucket)
  root_prefix <- store$resources$gcp$prefix %|||%
    store$resources$prefix %|||%
    path_store_default()
  prefix <- path_objects_dir(path_store = root_prefix)
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  key <- file.path(prefix, name)
  tar_assert_nzchar(key)
  bucket <- paste0("bucket=", bucket)
  key <- paste0("key=", key)
  c(bucket, key)
}

store_gcp_bucket <- function(path) {
  store_gcp_path_field(path = path, pattern = "^bucket=")
}

store_gcp_key <- function(path) {
  store_gcp_path_field(path = path, pattern = "^key=")
}

store_gcp_version <- function(path) {
  out <- store_gcp_path_field(path = path, pattern = "^version=")
  if_any(length(out) && nzchar(out), out, NULL)
}

store_gcp_path_field <- function(path, pattern) {
  keyvalue_field(x = path, pattern = pattern)
}

# Semi-automated tests of GCP GCS integration live in tests/gcp/. # nolint
# These tests should not be fully automated because they
# automatically create buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_read_object.tar_gcp <- function(store) {
  path <- store$file$path
  key <- store_gcp_key(path)
  bucket <- store_gcp_bucket(path)
  scratch <- path_scratch_temp_network(pattern = basename(store_gcp_key(path)))
  on.exit(unlink(scratch))
  dir_create(dirname(scratch))
  gcp_gcs_download(
    key = key,
    bucket = bucket,
    file = scratch,
    version = store_gcp_version(path),
    verbose = store$resources$gcp$verbose,
    max_tries = store$resources$gcp$max_tries
  )
  store_convert_object(store, store_read_path(store, scratch))
}

#' @export
store_exist_object.tar_gcp <- function(store, name = NULL) {
  path <- store$file$path
  gcp_gcs_exists(
    key = store_gcp_key(path),
    bucket = store_gcp_bucket(path),
    version = store_gcp_version(path),
    verbose = store$resources$gcp$verbose %|||% FALSE,
    max_tries = store$resources$gcp$max_tries %|||% 5L
  )
}

#' @export
store_delete_object.tar_gcp <- function(store, name = NULL) {
  path <- store$file$path
  key <- store_gcp_key(path)
  bucket <- store_gcp_bucket(path)
  version <- store_gcp_version(path)
  message <- paste(
    "could not delete target %s from gcp bucket %s key %s.",
    "Either delete the object manually in the gcp web console",
    "or call tar_invalidate(%s) to prevent the targets package",
    "from trying to delete it.\nMessage: "
  )
  message <- sprintf(message, name, bucket, key, name)
  tryCatch(
    gcp_gcs_delete(
      key = key,
      bucket =  bucket,
      version = version,
      verbose = store$resources$gcp$verbose %|||% FALSE,
      max_tries = store$resources$gcp$max_tries %|||% 5L
    ),
    error = function(condition) {
      tar_throw_validate(message, conditionMessage(condition))
    }
  )
}

# TODO: implement like store_delete_objects.tar_aws() with true batching
#   when https://github.com/cloudyr/googleCloudStorageR/issues/188 is solved.
#' @export
store_delete_objects.tar_gcp <- function(store, meta, batch_size, verbose) {
  gcp <- store$resources$gcp
  meta$bucket_group <- map_chr(
    x = meta$path,
    f = ~paste(
      store_gcp_bucket(.x),
      sep = "|"
    )
  )
  for (group in unique(meta$bucket_group)) {
    subset <- meta[meta$bucket_group == group,, drop = FALSE] # nolint
    for (index in seq_len(nrow(subset))) {
      example_path <- subset$path[[index]]
      bucket <- store_gcp_bucket(example_path)
      key <- store_gcp_key(example_path)
      version <- store_gcp_version(example_path)
      message <- paste(
        "could not object %s from gcp bucket %s.",
        "You may need to delete it manually.\nMessage: "
      )
      message <- sprintf(message, key, bucket)
      if (verbose) {
        tar_message_run(
          sprintf(
            "Deleting object %s%s in bucket %s",
            key,
            if_any(is.null(version), "", paste(" version", version)),
            bucket
          )
        )
      }
      tryCatch(
        gcp_gcs_delete(
          key = key,
          bucket =  bucket,
          version = version,
          verbose = gcp$verbose %|||% FALSE,
          max_tries = gcp$max_tries %|||% 5L
        ),
        error = function(condition) {
          tar_throw_validate(message, conditionMessage(condition))
        }
      )
    }
  }
}

#' @export
store_upload_object.tar_gcp <- function(store) {
  on.exit(unlink(store$file$stage, recursive = TRUE, force = TRUE))
  store_upload_object_gcp(store)
}

store_upload_object_gcp <- function(store) {
  key <- store_gcp_key(store$file$path)
  bucket <- store_gcp_bucket(store$file$path)
  head <- if_any(
    file_exists_stage(store$file),
    gcp_gcs_upload(
      file = store$file$stage,
      key = key,
      bucket = bucket,
      predefined_acl = store$resources$gcp$predefined_acl %|||% "private",
      verbose = store$resources$gcp$verbose %|||% FALSE,
      max_tries = store$resources$gcp$max_tries %|||% 5L
    ),
    tar_throw_file(
      "Cannot upload non-existent gcp staging file ",
      store$file$stage,
      " to key ",
      key,
      ". The target probably encountered an error."
    )
  )
  path <- grep(
    pattern = "^version=",
    x = store$file$path,
    value = TRUE,
    invert = TRUE
  )
  store$file$path <- c(path, paste0("version=", head$generation))
  store$file$hash <- digest_chr64(head$md5)
  invisible()
}

#' @export
store_has_correct_hash.tar_gcp <- function(store) {
  hash <- store_gcp_hash(store = store)
  !is.null(hash) && identical(hash, store$file$hash)
}

store_gcp_hash <- function(store) {
  tar_runtime$inventories$gcp <- tar_runtime$inventories$gcp %|||%
    inventory_gcp_init()
  tar_runtime$inventories$gcp$get_cache(store = store)
}
# nocov end

#' @export
store_get_packages.tar_gcp <- function(store) {
  c("googleCloudStorageR", NextMethod())
}
