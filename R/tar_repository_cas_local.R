#' @title Local content-addressable storage (CAS) repository
#'   (an experimental feature).
#' @export
#' @family content-addressable storage
#' @description Local content-addressable storage (CAS) repository.
#' @details Pass to the `repository` argument of [tar_target()] or
#'   [tar_option_set()] to use a local CAS system.
#' @inheritSection tar_repository_cas Content-addressable storage
#' @return A character string from [tar_repository_cas()] which may be
#'   passed to the `repository` argument of [tar_target()] or
#'   [tar_option_set()] to use a local CAS system.
#' @inheritParams tar_repository_cas
#' @param path Character string, file path to the CAS repository
#'   where all the data object files will be stored. `NULL` to default to
#'    `file.path(tar_config_get("store"), "cas")` (usually `"_targets/cas/"`).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
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
#' tar_make()
#' tar_read(y)
#' tar_read(z)
#' list.files("cas")
#' tar_meta(any_of(c("x", "z")), fields = any_of("data"))
#' })
#' }
tar_repository_cas_local <- function(
  path = NULL,
  consistent = FALSE
) {
  tar_assert_scalar(path %|||% "x")
  tar_assert_chr(path %|||% "x")
  tar_assert_nzchar(path %|||% "x")
  tar_assert_scalar(consistent)
  tar_assert_lgl(consistent)
  tar_assert_none_na(consistent)
  tar_repository_cas(
    upload = function(key, path) targets::tar_cas_u(cas, key, path),
    download = function(key, path) targets::tar_cas_d(cas, key, path),
    exists = function(key) targets::tar_cas_e(cas, key),
    consistent = consistent,
    substitute = list(cas = path)
  )
}

#' @title Local CAS upload.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @details The short function name helps reduce the size of the
#'   [tar_repository_cas()] format string and save space in the metadata.
#' @return Called for its side effects.
#' @param cas File path to the CAS repository. `NULL` to default to
#'    `file.path(tar_config_get("store"), "cas")` (usually `"_targets/cas/"`).
#' @param key Key of the object in the CAS system.
#' @param path Staging path of the file.
tar_cas_u <- function(cas, key, path) {
  cas <- cas %|||% path_cas_dir(tar_runtime$store)
  to <- file.path(cas, key)
  if (!file.exists(to)) {
    # Defined in R/utils_files.R. Works on both files and directories.
    file_move(path, to)
  }
}

#' @title Local CAS download.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @return Called for its side effects.
#' @inheritParams tar_cas_u
tar_cas_d <- function(cas, key, path) {
  cas <- cas %|||% path_cas_dir(tar_runtime$store)
  # Defined in R/utils_files.R. Works on both files and directories.
  file_copy(file.path(cas, key), path)
}

#' @title Existence check in local CAS.
#' @export
#' @keywords internal
#' @description For internal use only.
#' @details The short function name helps reduce the size of the
#'   [tar_repository_cas()] format string and save space in the metadata.
#' @details [tar_cas_e()] uses an in-memory cache
#'   in a package internal environment to maintain a list of keys that
#'   exists. This avoids expensive one-time lookups to the file system
#'   during [tar_make()].
#' @return `TRUE` if the key exists in the CAS system, `FALSE` otherwise.
#' @inheritParams tar_cas_u
tar_cas_e <- function(cas, key) {
  cas <- cas %|||% path_cas_dir(tar_runtime$store)
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
