#' @export
store_class_repository.aws <- function(repository, store, format) {
  format <- gsub(pattern = "\\&.*$", replacement = "", x = format)
  c(
    sprintf("tar_aws_%s", format),
    "tar_aws",
    "tar_cloud",
    if_any("tar_external" %in% class(store), character(0), "tar_external"),
    class(store)
  )
}

#' @export
store_assert_repository_setting.aws <- function(repository) {
}

#' @export
store_produce_path.tar_aws <- function(store, name, object, path_store) {
  store_produce_aws_path(
    store = store,
    name = name,
    object = object,
    path_store = path_store
  )
}

store_produce_aws_path <- function(store, name, object, path_store) {
  bucket <- store$resources$aws$bucket %|||% store$resources$bucket
  region <- store$resources$aws$region %|||% store$resources$region
  endpoint <- store$resources$aws$endpoint %|||% store$resources$endpoint
  tar_assert_nonempty(bucket)
  tar_assert_chr(bucket)
  tar_assert_scalar(bucket)
  tar_assert_nzchar(bucket)
  tar_assert_nonempty(region %|||% "region")
  tar_assert_chr(region %|||% "region")
  tar_assert_scalar(region %|||% "region")
  tar_assert_nonempty(endpoint %|||% "endpoint")
  tar_assert_chr(endpoint %|||% "endpoint")
  tar_assert_scalar(endpoint %|||% "endpoint")
  root_prefix <- store$resources$aws$prefix %|||%
    store$resources$prefix %|||%
    path_store_default()
  prefix <- path_objects_dir(path_store = root_prefix)
  tar_assert_nonempty(prefix)
  tar_assert_chr(prefix)
  tar_assert_scalar(prefix)
  key <- file.path(prefix, name)
  tar_assert_nzchar(key)
  bucket <- paste0("bucket=", bucket)
  region <- paste0("region=", if_any(is.null(region), "NULL", region))
  endpoint <- if_any(is.null(endpoint), "NULL", endpoint)
  endpoint <- base64url::base64_urlencode(endpoint)
  endpoint <- paste0("endpoint=", endpoint)
  key <- paste0("key=", key)
  c(bucket, region, key, endpoint)
}

store_aws_bucket <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(path[1L])
  }
  # with metadata written by targets > 0.8.1:
  store_aws_path_field(path = path, pattern = "^bucket=")
}

store_aws_region <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(NULL)
  }
  # with metadata written by targets > 0.8.1:
  out <- store_aws_path_field(path = path, pattern = "^region=")
  out <- if_any(length(out) > 0L && any(nzchar(out)), out, "")
  if_any(identical(out, "NULL"), NULL, out)
}

store_aws_endpoint <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(NULL)
  }
  # with metadata written by targets > 0.8.1:
  out <- store_aws_path_field(path = path, pattern = "^endpoint=")
  out <- if_any(length(out) > 0L && any(nzchar(out)), out, "")
  out <- base64url::base64_urldecode(out)
  if_any(identical(out, "NULL"), NULL, out)
}

store_aws_key <- function(path) {
  # compatibility with targets <= 0.8.1:
  if (store_aws_path_0.8.1(path)) {
    return(path[2L])
  }
  store_aws_path_field(path = path, pattern = "^key=")
}

store_aws_version <- function(path) {
  out <- store_aws_path_field(path = path, pattern = "^version=")
  if_any(length(out) && nzchar(out), out, NULL)
}

store_aws_path_field <- function(path, pattern) {
  path <- store_aws_split_colon(path)
  keyvalue_field(x = path, pattern = pattern)
}

store_aws_path_0.8.1 <- function(path) {
  !any(grepl(pattern = "^bucket=", x = path))
}

# Tech debt from a dev version. Need to be compatible.
store_aws_split_colon <- function(path) {
  index <- grep(pattern = "^bucket=", x = path)
  bucket_pair <- unlist(strsplit(x = path[index], split = ":"))
  c(bucket_pair, path[-index])
}

# Semi-automated tests of Amazon S3 integration live in tests/aws/. # nolint
# These tests should not be fully automated because they
# automatically create S3 buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
#' @export
store_read_object.tar_aws <- function(store) {
  path <- store$file$path
  key <- store_aws_key(path)
  bucket <- store_aws_bucket(path)
  scratch <- path_scratch_temp_network(pattern = basename(store_aws_key(path)))
  on.exit(unlink(scratch))
  dir_create(dirname(scratch))
  aws <- store$resources$aws
  aws_s3_download(
    key = key,
    bucket = bucket,
    file = scratch,
    region = store_aws_region(path),
    endpoint = store_aws_endpoint(path),
    version = store_aws_version(path),
    args = aws$args,
    max_tries = aws$max_tries,
    seconds_timeout = aws$seconds_timeout,
    close_connection = aws$close_connection,
    s3_force_path_style = aws$s3_force_path_style
  )
  store_convert_object(store, store_read_path(store, scratch))
}

#' @export
store_exist_object.tar_aws <- function(store, name = NULL) {
  path <- store$file$path
  aws <- store$resources$aws
  head <- aws_s3_exists(
    key = store_aws_key(path),
    bucket = store_aws_bucket(path),
    region = store_aws_region(path),
    endpoint = store_aws_endpoint(path),
    version = store_aws_version(path),
    args = aws$args,
    max_tries = aws$max_tries,
    seconds_timeout = aws$seconds_timeout,
    close_connection = aws$close_connection,
    s3_force_path_style = aws$s3_force_path_style
  )
  !is.null(head)
}

#' @export
store_delete_objects.tar_aws <- function(store, meta, batch_size, verbose) {
  aws <- store$resources$aws
  meta$bucket_group <- map_chr(
    x = meta$path,
    f = ~paste(
      store_aws_bucket(.x),
      store_aws_region(.x),
      store_aws_endpoint(.x),
      sep = "|"
    )
  )
  for (group in unique(meta$bucket_group)) {
    subset <- meta[meta$bucket_group == group,, drop = FALSE] # nolint
    example_path <- subset$path[[1L]]
    bucket <- store_aws_bucket(example_path)
    region <- store_aws_region(example_path)
    endpoint <- store_aws_endpoint(example_path)
    objects <- map(
      subset$path,
      ~list(
        Key = store_aws_key(.x),
        VersionId = store_aws_version(.x)
      )
    )
    message <- paste(
      "could not delete one or more objects from AWS bucket %s.",
      "You may need to delete them manually.\nMessage: "
    )
    message <- sprintf(message, bucket)
    tryCatch(
      aws_s3_delete_objects(
        objects = objects,
        bucket =  bucket,
        batch_size = batch_size,
        region = region,
        endpoint = endpoint,
        args = aws$args,
        max_tries = aws$max_tries,
        seconds_timeout = aws$seconds_timeout,
        close_connection = aws$close_connection,
        s3_force_path_style = aws$s3_force_path_style,
        verbose = verbose
      ),
      error = function(condition) {
        tar_throw_validate(message, conditionMessage(condition))
      }
    )
  }
}

#' @export
store_upload_object.tar_aws <- function(store) {
  on.exit(unlink(store$file$stage, recursive = TRUE, force = TRUE))
  store_upload_object_aws(store)
}

store_upload_object_aws <- function(store) {
  key <- store_aws_key(store$file$path)
  bucket <- store_aws_bucket(store$file$path)
  aws <- store$resources$aws
  head <- if_any(
    file_exists_stage(store$file),
    aws_s3_upload(
      file = store$file$stage,
      key = key,
      bucket = bucket,
      region = store_aws_region(store$file$path),
      endpoint = store_aws_endpoint(store$file$path),
      part_size = aws$part_size,
      args = aws$args,
      max_tries = aws$max_tries,
      seconds_timeout = aws$seconds_timeout,
      close_connection = aws$close_connection,
      s3_force_path_style = aws$s3_force_path_style
    ),
    tar_throw_file(
      "Cannot upload non-existent AWS staging file ",
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
  store$file$path <- c(path, paste0("version=", head$VersionId))
  store$file$hash <- hash_object(head$ETag)
  invisible()
}

#' @export
store_has_correct_hash.tar_aws <- function(store) {
  hash <- store_aws_hash(store)
  !is.null(hash) && identical(hash, store$file$hash)
}

store_aws_hash <- function(store) {
  tar_runtime$inventories$aws <- tar_runtime$inventories$aws %|||%
    inventory_aws_init()
  tar_runtime$inventories$aws$get_cache(store = store)
}
# nocov end

#' @export
store_get_packages.tar_aws <- function(store) {
  c("paws.common", "paws.storage", NextMethod())
}
