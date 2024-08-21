#' @title Define a custom content-addressable storage (CAS) repository.
#' @export
#' @family targets
#' @description Define a custom storage repository that uses
#'   content-addressable storage (CAS).
#' @details Repository functions:
#'   In [tar_repository_cas()], functions `upload`, `download`,
#'   and `exists` must be perfectly pure
#'   and perfectly self-sufficient.
#'   They must load or namespace all their own packages,
#'   and they must not depend on any custom user-defined
#'   functions or objects in the global environment of your pipeline.
#'   `targets` converts each function to and from text,
#'   so it must not rely on any data in the closure.
#'   This disqualifies functions produced by `Vectorize()`,
#'   for example.
#'
#'   Some functions may need to be adapted and configured based on other
#'   inputs. For example, you may want to define
#'   `upload = \(path, key) file.move(path, file.path(folder, key))`
#'   but do not want to hard-code a value of `folder` when you write the
#'   underlying function. `substitute()` can help inject values as needed.
#'   For example:
#'
#'   ```
#'   upload = substitute(
#'     \(path, key) file.move(path, file.path(folder, key)),
#'     list(folder = "my_cas")
#'   )
#'   ```
#' @param upload A function with arguments `path` and `key`, in that order.
#'   This function should upload the file or directory at `path`
#'   to the CAS system. `key` denotes the name of the destination data object
#'   in the CAS system.
#' @param download A function with arguments `path` and `key`, in that order.
#'   This function should download the data object at `key` from
#'   the CAS system to the file or directory at `path`.
#'   to the CAS system. `key` denotes the name of the destination data object
#'   in the CAS system.
#' @param exists A function with a single argument `key`.
#'   This function should check if there is an object at `key` in
#'   the CAS system.'
tar_repository_cas <- function(
  upload,
  download,
  exists
) {
  tar_assert_function(upload)
  tar_assert_function(download)
  tar_assert_function(exists)
  tar_assert_function_arguments(upload, c("path", "key"))
  tar_assert_function_arguments(download, c("path", "key"))
  tar_assert_function_arguments(exists, "key")
  paste(
    "repository_cas",
    tar_repository_cas_field("upload", upload),
    tar_repository_cas_field("download", download),
    tar_repository_cas_field("exists", exists),
    sep = "&"
  )
}

tar_repository_cas_field <- function(key, value) {
  encoded <- base64url::base64_urlencode(tar_deparse_safe(value))
  paste0(key, "=", encoded)
}
