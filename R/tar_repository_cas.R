#' @title Define a custom content-addressable storage (CAS) repository.
#' @export
#' @family storage
#' @description Define a custom storage repository that uses
#'   content-addressable storage (CAS).
#' @details See the [tar_repository_cas_local()] function for a
#'   sophisticated CAS system based on a local folder on disk.
#'   It uses [tar_repository_cas_local_upload()],
#'   [tar_repository_cas_local_download()], and
#'   [tar_repository_cas_local_exists()] for the respective
#'   `upload`, `download`, and `exists` methods.
#'   See the "Repository functions" section for specific advice on how
#'   to write your own methods.
#' @section Repository functions:
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
#'   `upload` and `download` should also handle directory outputs in
#'   some way. Either they should assume `path` can be a directory
#'   as well as a file, or `upload` should throw an error if `path`
#'   is a directory.
#'
#'   Some functions may need to be adapted and configured based on other
#'   inputs. For example, you may want to define
#'   `upload = \(key, path) file.move(path, file.path(folder, key))`
#'   but do not want to hard-code a value of `folder` when you write the
#'   underlying function. `substitute()` can help inject values into the
#'   body of a function. For example:
#'
#'   ```
#'   upload <-  \(key, path) {}
#'   body(upload) <- substitute(
#'     file.move(path, file.path(folder, key)),
#'     list(folder = "my_cas")
#'   )
#'   print(upload)
#'   ```
#'
#'   Temporary or sensitive such as authentication credentials
#'   should not be injected
#'   this way into the function body. Instead, pass them as environment
#'   variables using [tar_resources_repository_cas()].
#' @param upload A function with arguments `key` and `path`, in that order.
#'   This function should upload the file or directory from `path`
#'   to the CAS system.
#'   `path` is where the file is originally saved to disk outside the CAS
#'   system. It could be a staging area or a custom `format = "file"`
#'   location. `key` denotes the name of the destination data object
#'   in the CAS system.
#'
#'   `upload` should usually copy the file to its destination instead of
#'   removing the original source path. This is because `format = "file"`
#'   targets should remain on disk after the upload phase. For non-"file"
#'   formats, `targets` will remove the temporary staging file
#'   automatically after the upload is finished. If you are working on your
#'   local machine and have need for a CAS system, it is likely that the
#'   CAS object files will exist on a different drive than your local
#'   file environment anyway, which means copying will probably have to
#'   take place regardless.
#' @param download A function with arguments `key` and `path`, in that order.
#'   This function should download the data object at `key` from
#'   the CAS system to the file or directory at `path`.
#'   `key` denotes the name of the data object in the CAS system.
#'   `path` is a temporary staging area and not the final destination.
#' @param exists A function with a single argument `key`.
#'   This function should check if there is an object at `key` in
#'   the CAS system.
#'
#'   For efficiency, `exists` can maintain an in-memory cache of keys.
#'   New lookups can check the cache and potentially avoid expensive
#'   queries to the CAS system. See the source code of
#'   [tar_repository_cas_local_exists()]
#'   for an example of how this can work for a local file system CAS.
#' @param consistent Logical. Set to `TRUE` if the storage platform is
#'   strongly read-after-write consistent. Set to `FALSE` if the platform
#'   is not necessarily strongly read-after-write consistent.
#'
#'   A data storage system is said to have strong read-after-write consistency
#'   if a new object is fully available for reading as soon as the write
#'   operation finishes. Many modern cloud services like Amazon S3 and
#'   Google Cloud Storage have strong read-after-write consistency,
#'   meaning that if you upload an object with a PUT request, then a
#'   GET request immediately afterwards will retrieve the precise
#'   version of the object you just uploaded.
#'
#'   Some storage systems do not have strong read-after-write consistency.
#'   One example is network file systems (NFS). On a computing cluster,
#'   if one node creates a file on an NFS, then there is a delay before
#'   other nodes can access the new file. `targets` handles this situation
#'   by waiting for the new file to appear with the correct hash
#'   before attempting to use it in downstream computations.
#'   `consistent = FALSE` imposes a waiting period in which `targets`
#'   repeatedly calls the `exists` method until the file becomes available
#'   (at time intervals configurable with [tar_resources_network()]).
#'   These extra calls to `exists` may come with a
#'   little extra latency / computational burden,
#'   but on systems which are not strongly read-after-write consistent,
#'   it is the only way `targets` can safely use the current results
#'   for downstream computations.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   repository <- tar_repository_cas(
#'     upload = function(key, path) {
#'       if (dir.exists(path)) {
#'         stop("This CAS repository does not support directory outputs.")
#'       }
#'       if (!file.exists("cas")) {
#'         dir.create("cas", recursive = TRUE)
#'       }
#'       file.copy(path, file.path("cas", key))
#'     },
#'     download = function(key, path) {
#'       file.copy(file.path("cas", key), path)
#'     },
#'     exists = function(key) {
#'       file.exists(file.path("cas", key))
#'     }
#'   )
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
tar_repository_cas <- function(
  upload,
  download,
  exists,
  consistent = FALSE
) {
  tar_assert_function(upload)
  tar_assert_function(download)
  tar_assert_function(exists)
  tar_assert_function_arguments(upload, c("key", "path"))
  tar_assert_function_arguments(download, c("key", "path"))
  tar_assert_function_arguments(exists, "key")
  tar_assert_scalar(consistent)
  tar_assert_lgl(consistent)
  tar_assert_none_na(consistent)
  paste(
    "repository_cas",
    tar_repository_cas_field("upload", upload),
    tar_repository_cas_field("download", download),
    tar_repository_cas_field("exists", exists),
    tar_repository_cas_field("consistent", consistent),
    sep = "&"
  )
}

tar_repository_cas_field <- function(key, value) {
  encoded <- base64url::base64_urlencode(tar_deparse_safe(value))
  paste0(key, "=", encoded)
}
