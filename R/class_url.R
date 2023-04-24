# Tested in tests/interactive/test-class_url.R,
# not in testthat due to unreliable URLs.
# nocov start
#' @export
store_new.url <- function(format, file = NULL, resources = NULL) {
  store_url_new(file, resources)
}

store_url_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(environment(), c("tar_url", "tar_external", "tar_store"))
}

#' @export
store_assert_format_setting.url <- function(format) {
}

#' @export
store_read_path.tar_url <- function(store, path) {
  path[!is.na(path)]
}

#' @export
store_write_object.tar_url <- function(store, object) {
}

#' @export
store_write_path.tar_url <- function(store, object, path) {
}

#' @export
store_produce_path.tar_url <- function(store, name, object, path_store) {
  object
}

#' @export
store_convert_object.tar_url <- function(store, object) {
  as.character(object)
}

#' @export
store_assert_format.tar_url <- function(store, object, name) {
  if (!is.character(object %|||% character(0))) {
    tar_throw_validate(
      "target ", name, " did not return a character. ",
      "targets with format = \"url\" must return ",
      "character vectors of URL paths."
    )
  }
}

#' @export
store_hash_early.tar_url <- function(store) { # nolint
  store$file$hash <- url_hash(
    url = store$file$path,
    handle = store$resources$url$handle %|||% store$resources$handle,
    seconds_interval = store$resources$url$seconds_interval %|||% 0.1,
    seconds_timeout = store$resources$url$seconds_timeout %|||% 5
  )
}

#' @export
store_hash_late.tar_url <- function(store) { # nolint
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
  handle <- store$resources$url$handle %|||% store$resources$handle
  seconds_interval <- store$resources$url$seconds_interval %|||% 0.1
  seconds_timeout <- store$resources$url$seconds_timeout %|||% 5
  all(
    url_exists(
      url = store$file$path,
      handle = handle,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
  ) &&
    identical(
      url_hash(
        url = store$file$path,
        handle = handle,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout
      ),
      store$file$hash
    )
}

#' @export
store_get_packages.tar_url <- function(store) {
  "curl"
}
# nocov end
