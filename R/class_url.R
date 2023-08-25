# Tested in tests/interactive/test-class_url.R,
# not in testthat due to unreliable URLs.
# nocov start
#' @export
store_class_format.url <- function(format) {
  c("tar_url", "tar_external", "tar_store")
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
  handle <- store$resources$url$handle %|||% store$resources$handle
  max_tries <- store$resources$url$max_tries %|||% 5L
  seconds_interval <- store$resources$url$seconds_interval %|||% 1
  seconds_timeout <- store$resources$url$seconds_timeout %|||% 60
  verbose <- store$resources$url$verbose %|||% TRUE
  store$file$hash <- url_hash(
    url = store$file$path,
    handle = handle,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    verbose = verbose
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
  identical(
    url_hash(
      url = store$file$path,
      handle = handle,
      seconds_interval = 0,
      seconds_timeout = 0,
      max_tries = 1L,
      verbose = TRUE
    ),
    store$file$hash
  )
}

#' @export
store_get_packages.tar_url <- function(store) {
  "curl"
}
# nocov end
