#' @title Local content-addressable storage (CAS) repository
#'   (an experimental feature).
#' @export
#' @family storage
#' @description Local content-addressable storage (CAS) repository.
#' @details Pass to the `repository` argument of [tar_target()] or
#'   [tar_option_set()] to use a local CAS system.
#' @return A character string from [tar_repository_cas()] which may be
#'   passed to the `repository` argument of [tar_target()] or
#'   [tar_option_set()] to use a local CAS system.
#' @inheritParams tar_repository_cas
#' @param path Character string, file path to the CAS repository
#'   where all the data object files will be stored.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   repository <- tar_repository_cas_local("cas")
#'   write_file <- function(object) {
#'     writeLines(as.character(object), "file.txt")
#'     "file.txt"
#'   }
#'   list(
#'     tar_target(x, c(2L, 4L), repository = repository),
#'     tar_target(
#'       y,
#'       x,
#'       pattern = map(x),
#'       format = "qs",
#'       repository = repository
#'     ),
#'     tar_target(z, write_file(y), format = "file", repository = repository)
#'   )
#' })
#' tar_make(callr_function = NULL)
#' tar_read(y)
#' tar_read(z)
#' list.files("cas")
#' tar_meta(any_of(c("x", "z")), fields = any_of("data"))
#' })
#' }
tar_repository_cas_local <- function(
  path = file.path(targets::tar_config_get("store"), "cas"),
  consistent = FALSE
) {
  tar_assert_scalar(path)
  tar_assert_chr(path)
  tar_assert_nzchar(path)
  tar_assert_scalar(consistent)
  tar_assert_lgl(consistent)
  tar_assert_none_na(consistent)
  data <- list(cas = path)
  upload <- function(key, path) {
  }
  download <- function(key, path) {
  }
  exists <- function(key) {
  }
  body(upload) <- substitute(
    targets::tar_repository_cas_local_upload(cas, key, path),
    env = data
  )
  body(download) <- substitute(
    targets::tar_repository_cas_local_download(cas, key, path),
    env = data
  )
  body(exists) <- substitute(
    targets::tar_repository_cas_local_exists(cas, key),
    env = data
  )
  tar_repository_cas(
    upload = upload,
    download = download,
    exists = exists,
    consistent = consistent
  )
}

#' @title Local CAS upload.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @return Called for its side effects.
#' @param cas File path to the CAS repository.
#' @param key Key of the object in the CAS system.
#' @param path Staging path of the file.
tar_repository_cas_local_upload <- function(cas, key, path) {
  to <- file.path(cas, key)
  if (!file.exists(to)) {
    # Defined in R/utils_files.R. Works on both files and directories.
    file_copy(path, to)
  }
}

#' @title Local CAS download.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @return Called for its side effects.
#' @inheritParams tar_repository_cas_local_upload
tar_repository_cas_local_download <- function(cas, key, path) {
  # Defined in R/utils_files.R. Works on both directories.
  file_copy(file.path(cas, key), path)
}

#' @title Existence check in local CAS.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @details [tar_repository_cas_local_exists()] uses an in-memory cache
#'   in a package internal environment to maintain a list of keys that
#'   exists. This avoids expensive one-time lookups to the file system
#'   during [tar_make()].
#' @return `TRUE` if the key exists in the CAS system, `FALSE` otherwise.
#' @inheritParams tar_repository_cas_local_upload
tar_repository_cas_local_exists <- function(cas, key) {
  if (is.null(tar_repository_cas_local_cache[[cas]])) {
    keys <- list.files(cas)
    data <- rep(TRUE, length(keys))
    names(data) <- keys
    tar_repository_cas_local_cache[[cas]] <- list2env(as.list(data))
  }
  if (isTRUE(tar_repository_cas_local_cache[[cas]][[key]])) {
    return(TRUE)
  }
  out <- file.exists(file.path(cas, key))
  if (out) {
    tar_repository_cas_local_cache[[cas]][[key]] <- out
  }
  out
}

tar_repository_cas_local_cache <- new.env(parent = emptyenv(), hash = TRUE)
