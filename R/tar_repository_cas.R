#' @title Define a custom content-addressable storage
#'   (CAS) repository (an experimental feature).
#' @export
#' @family content-addressable storage
#' @description Define a custom storage repository that uses
#'   content-addressable storage (CAS).
#' @section Content-addressable storage:
#'   Normally, `targets` organizes output data
#'   based on target names. For example,
#'   if a pipeline has a single target `x` with default settings,
#'   then [tar_make()] saves the output data to the file
#'   `_targets/objects/x`. When the output of `x` changes, [tar_make()]
#'   overwrites `_targets/objects/x`.
#'   In other words, no matter how many changes happen to `x`,
#'   the data store always looks like this:
#'
#'   ```
#'   _targets/
#'       meta/
#'           meta
#'       objects/
#'           x
#'   ```
#'
#'   By contrast, with content-addressable storage (CAS),
#'   `targets` organizes outputs based on the hashes of their contents.
#'   The name of each output file is its hash, and the
#'   metadata maps these hashes to target names. For example, suppose
#'   target `x` has `repository = tar_repository_cas_local("my_cas")`.
#'   When the output of `x` changes, [tar_make()] creates a new file
#'   inside `my_cas/` without overwriting or deleting any other files
#'   in that folder. If you run [tar_make()] three different times
#'   with three different values of `x`, then storage will look like this:
#'
#'   ```
#'   _targets/
#'       meta/
#'           meta
#'   my_cas/
#'       1fffeb09ad36e84a
#'       68328d833e6361d3
#'       798af464fb2f6b30
#'   ```
#'
#'   The next call to `tar_read(x)` uses `tar_meta(x)$data`
#'   to look up the current hash of `x`. If `tar_meta(x)$data` returns
#'   `"1fffeb09ad36e84a"`, then `tar_read(x)` returns the data from
#'   `my_cas/1fffeb09ad36e84a`. Files `my_cas/68328d833e6361d3` and
#'   and `my_cas/798af464fb2f6b30` are left over from previous values of `x`.
#'
#'   Because CAS accumulates historical data objects,
#'   it is ideal for data versioning and collaboration.
#'   If you commit the `_targets/meta/meta` file to version control
#'   alongside the source code,
#'   then you can revert to a previous state of your pipeline with all your
#'   targets up to date, and a colleague can leverage your hard-won
#'   results using a fork of your code and metadata.
#'
#'   The downside of CAS is the cost of accumulating many data objects
#'   over time. Most pipelines that use CAS
#'   should have a garbage collection system or retention policy
#'   to remove data objects when they no longer needed.
#'
#'   The [tar_repository_cas()] function lets you create your own CAS system
#'   for `targets`. You can supply arbitrary custom methods to upload,
#'   download, and check for the existence of data objects. Your custom
#'   CAS system can exist locally on a shared file system or remotely
#'   on the cloud (e.g. in an AWS S3 bucket).
#'   See the "Repository functions" section and the documentation
#'   of individual arguments for advice on how
#'   to write your own methods.
#'
#'   The [tar_repository_cas_local()] function has an example
#'   CAS system based on a local folder on disk.
#'   It uses [tar_cas_u()] for uploads,
#'   [tar_cas_d()] for downloads, and
#'   [tar_cas_l()] for listing keys.
#' @section Repository functions:
#'   In [tar_repository_cas()], functions `upload`, `download`,
#'   `exists`, and `keys` must be completely pure and self-sufficient.
#'   They must load or namespace all their own packages,
#'   and they must not depend on any custom user-defined
#'   functions or objects in the global environment of your pipeline.
#'   `targets` converts each function to and from text,
#'   so it must not rely on any data in the closure.
#'   This disqualifies functions produced by `Vectorize()`,
#'   for example.
#'
#'   `upload` and `download` can assume `length(path)` is 1, but they should
#'   account for the possibility that `path` could be a directory. To simply
#'   avoid supporting directories, `upload` could simply call an assertion:
#'
#'   ```r
#'   targets::tar_assert_not_dir(
#'     path,
#'     msg = "This CAS upload method does not support directories."
#'   )
#'   ```
#'
#'   Otherwise, support for directories may require handling them as a
#'   special case. For example, `upload` and `download` could copy
#'   all the files in the given directory,
#'   or they could manage the directory as a zip archive.
#'
#'   Some functions may need to be adapted and configured based on other
#'   inputs. For example, you may want to define
#'   `upload = \(key, path) file.rename(path, file.path(folder, key))`
#'   but do not want to hard-code a value of `folder` when you write the
#'   underlying function. The `substitute` argument handles this situation.
#'   For example, if `substitute` is `list(folder = "my_folder")`,
#'   then `upload` will end up as
#'   `\(key, path) file.rename(path, file.path("my_folder", key))`.
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
#'   To differentiate between
#'   `format = "file"` targets and non-`"file"` targets, the `upload`
#'   method can use [tar_format_get()]. For example, to make
#'   [tar_repository_cas_local()] efficient, `upload` moves the file
#'   if `targets::tar_format_get() == "file"` and copies it otherwise.
#'
#'   See the "Repository functions" section for more details.
#' @param download A function with arguments `key` and `path`, in that order.
#'   This function should download the data object at `key` from
#'   the CAS system to the file or directory at `path`.
#'   `key` denotes the name of the data object in the CAS system.
#'   `path` is a temporary staging area and not the final destination.
#'
#'   Please be careful to avoid deleting the object at `key` from the CAS
#'   system. If the CAS system is a local file system, for example,
#'   `download` should copy the file and not simply move it
#'   (e.g. please avoid `file.rename()`).
#'
#'   See the "Repository functions" section for more details.
#' @param exists A function with a single argument `key`,
#'   where `key` is a single character string (`length(key)` is 1)
#'   to identify a single object in the CAS system.
#'
#'   The `exists` function should check if there is a single object at
#'   a single `key` in the CAS system.
#'   It is ignored if `list` is given and `consistent` is `TRUE`.
#'
#'   See the "Repository functions" section for more details.
#' @param list Either `NULL` or an optional function with a single
#'   argument named `keys`.
#'
#'   The `list` function increases efficiency by reducing repeated calls
#'   to the `exists` function (see above) or entirely avoiding them
#'   if `consistent` is `TRUE.
#'
#'   The `list` function should return a character vector of keys that
#'   already exist in the CAS system.
#'   The `keys` argument of `list` is a character vector of
#'   CAS keys (hashes) which are already recorded in the pipeline metadata
#'   (`tar_meta()`).
#'   For greater efficiency, the `list` function can restrict its query
#'   to these existing keys instead of trying to list the billions of keys
#'   that could exist in a CAS system.
#'   See the source code of [tar_cas_l()]
#'   for an example of how this can work for a local file system CAS.
#'
#'   See the "Repository functions" section for more details.
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
#' @param substitute Named list of values to be inserted into the
#'   body of each custom function in place of symbols in the body.
#'   For example, if
#'   `upload = function(key, path) do_upload(key, path, bucket = X)`
#'   and `substitute = list(X = "my_aws_bucket")`, then
#'   the `upload` function will actually end up being
#'   `function(key, path) do_upload(key, path, bucket = "my_aws_bucket")`.
#'
#'   Please do not include temporary or sensitive information
#'   such as authentication credentials.
#'   If you do, then `targets` will write them
#'   to metadata on disk, and a malicious actor could
#'   steal and misuse them. Instead, pass sensitive information
#'   as environment variables using [tar_resources_repository_cas()].
#'   These environment variables only exist in the transient memory
#'   spaces of the R sessions of the local and worker processes.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   repository <- tar_repository_cas(
#'     upload = function(key, path) {
#'       if (dir.exists(path)) {
#'         stop("This CAS repository does not support directory outputs.")
#'       }
#'       if (!file.exists("cas")) {
#'         dir.create("cas", recursive = TRUE)
#'       }
#'       file.rename(path, file.path("cas", key))
#'     },
#'     download = function(key, path) {
#'       file.copy(file.path("cas", key), path)
#'     },
#'     exists = function(key) {
#'       file.exists(file.path("cas", key))
#'     },
#'     list = function(keys) {
#'       keys[file.exists(file.path("cas", keys))]
#'     },
#'     consistent = FALSE
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
#' tar_make()
#' tar_read(y)
#' tar_read(z)
#' list.files("cas")
#' tar_meta(any_of(c("x", "z")), fields = any_of("data"))
#' })
#' }
tar_repository_cas <- function(
  upload,
  download,
  exists = NULL,
  list = NULL,
  consistent = FALSE,
  substitute = base::list()
) {
  tar_assert_scalar(consistent)
  tar_assert_lgl(consistent)
  tar_assert_none_na(consistent)
  tar_assert_function(upload)
  tar_assert_function(download)
  tar_assert_function_arguments(upload, c("key", "path"))
  tar_assert_function_arguments(download, c("key", "path"))
  list_function <- environment()$list
  if (!is.null(list_function) && consistent) {
    exists <- NULL
  } else {
    tar_assert_function(
      exists,
      msg = paste(
        "In tar_repository_cas(), 'exists' must be a function",
        "if 'list' is NULL or 'consistent' is 'FALSE'"
      )
    )
    tar_assert_function_arguments(exists, "key")
  }
  if (!is.null(list_function)) {
    tar_assert_function(list_function)
    tar_assert_function_arguments(list_function, "keys")
  }
  exists_field <- if_any(
    is.null(exists),
    tar_repository_cas_field("exists", NULL),
    tar_repository_cas_field("exists", tar_sub_body(exists, substitute))
  )
  list_field <- if_any(
    is.null(list_function),
    tar_repository_cas_field("list", NULL),
    tar_repository_cas_field("list", tar_sub_body(list_function, substitute))
  )
  paste(
    "repository_cas",
    tar_repository_cas_field("upload", tar_sub_body(upload, substitute)),
    tar_repository_cas_field("download", tar_sub_body(download, substitute)),
    exists_field,
    list_field,
    tar_repository_cas_field("consistent", consistent),
    sep = "&"
  )
}

tar_repository_cas_field <- function(key, value) {
  if (is.null(value)) {
    return(paste0(key, "="))
  }
  if (is.function(value)) {
    value <- body(value)
  }
  encoded <- base64url::base64_urlencode(tar_deparse_safe(value))
  paste0(key, "=", encoded)
}
