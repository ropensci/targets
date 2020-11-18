#' @export
store_new.url <- function(class, file = NULL, resources = NULL) {
  store_url_new(file, resources)
}

store_url_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_url", "tar_store"))
}

#' @export
store_assert_format_setting.url <- function(class) {
}

#' @export
store_read_path.tar_url <- function(store, path) {
  path
}

#' @export
store_write_object.tar_url <- function(store, object) {
}

#' @export
store_write_path.tar_url <- function(store, object, path) {
}

#' @export
store_produce_path.tar_url <- function(store, name, object) {
  object
}

#' @export
store_coerce_object.tar_url <- function(store, object) {
  as.character(object)
}

#' @export
store_assert_format.tar_url <- function(store, object) {
  if (!is.null(object) && !is.character(object)) {
    throw_validate(
      "targets with format = \"url\" must return ",
      "character vectors of URL paths."
    )
  }
}

#' @export
store_early_hash.tar_url <- function(store) { # nolint
  store$file$hash <- url_hash(
    url = store$file$path,
    handle = store$resources$handle
  )
}

#' @export
store_late_hash.tar_url <- function(store) { # nolint
}

#' @export
store_ensure_correct_hash.tar_url <- function( # nolint
  store,
  storage,
  deployment
) {
}

#' @export
store_sync_file_meta.tar_url <- function(store, target, meta) {
}

#' @export
store_has_correct_hash.tar_url <- function(store) {
  handle <- store$resources$handle
  all(url_exists(store$file$path, handle)) &&
    identical(url_hash(store$file$path, handle), store$file$hash)
}

#' @export
store_get_packages.tar_url <- function(store) {
  "curl"
}
