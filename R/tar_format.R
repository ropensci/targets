#' @title Define a custom target storage format.
#' @export
#' @family targets
#' @description Define a custom target storage format for the
#'   `format` argument of [tar_target()] or [tar_option_set()].
#' @return A character string encoding the custom format.
#'   You can supply this string directly to the `format`
#'   argument of [tar_target()] or [tar_option_set()].
#' @section Marshalling:
#'   If an object can only be used in the R session
#'   where it was created, it is called "non-exportable".
#'   Examples of non-exportable R objects are Keras models,
#'   Torch objects, `xgboost` matrices, `xml2` documents,
#'   `rstan` model objects, `sparklyr` data objects, and
#'   database connection objects. These objects cannot be
#'   exported to parallel workers (e.g. for [tar_make_future()])
#'   without special treatment. To send an non-exportable
#'   object to a parallel worker, the object must be marshalled:
#'   converted into a form that can be exported safely
#'   (similar to serialization but not always the same).
#'   Then, the worker must unmarshal the object: convert it
#'   into a form that is usable and valid in the current R session.
#'   Arguments `marshal` and `unmarshal` of `tar_format()`
#'   let you control how marshalling and unmarshalling happens.
#' @param read A function with a single argument named `path`.
#'   This function should read and return the target stored
#'   at the file in the argument.
#' @param write A function with two arguments: `object` and `path`,
#'   in that order. This function should save the R object `object`
#'   to the file path at `path`. The return value does not matter.
#' @param marshal A function with a single argument named `object`.
#'   This function should marshal the R object and return
#'   a value that can be exported to remote parallel workers.
#'   See the Marshalling section for details.
#' @param unmarshal A function with a single argument named `object`.
#'   This function should unmarshal the (marshalled) R object and return
#'   a value that is appropriate and valid for use
#'   on a parallel worker.
#'   See the Marshalling section for details.
#' @param cloud Character of length 1, `"none"` for local storage
#'   and `"aws"` for storage on Amazon S3. Read
#'   <https://books.ropensci.org/targets/storage_amazon.html>
#'   for more on Amazon S3 storage.
#' @examples
#' # The following target is equivalent to 
#' # tar_target(name, command(), format = "keras"):
#' tar_target(
#'   name,
#'   command(),
#'   format = tar_format(
#'     read = function(path) {
#'       keras::load_model_hdf5(path)
#'     },
#'     write = function(object, path) {
#'       keras::save_model_hdf5(object = object, filepath = path)
#'     },
#'     marshal = function(object) {
#'       keras::serialize_model(object)
#'     },
#'     unmarshal = function(object) {
#'       keras::unserialize_model(object)
#'     },
#'     cloud = "none" # Could be "aws" (equivalent to format = "aws_keras")
#'   )
#' )
#' x <- tar_target(x, download_data(), cue = tar_cue(mode = "always"))
tar_format <- function(
  read = function(path) {
    readRDS(path)
  },
  write = function(object, path) {
    saveRDS(object = object, file = path, version = 3L)
  },
  marshal = function(object) {
    identity(object)
  },
  unmarshal = function(object) {
    identity
  },
  cloud = c("none", "aws")
) {
  tar_assert_function(read)
  tar_assert_function(write)
  tar_assert_function(read)
  tar_assert_function(read)
  cloud <- match.arg(cloud)
  tar_assert_chr(cloud)
  tar_assert_scalar(cloud)
  tar_assert_nzchar(cloud)
  c(
    "custom",
    tar_format_field("read", read),
    tar_format_field("write", write),
    tar_format_field("marshal", marshal),
    tar_format_field("unmarshal", marshal),
    paste0("cloud=", match.arg(cloud))
  )
}

tar_format_field <- function(key, value) {
  paste0(key, "=", base64url::base64_urlencode(tar_deparse_safe(value)))
}
