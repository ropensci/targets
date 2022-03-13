#' @title Define a custom target storage format.
#' @export
#' @family targets
#' @description Define a custom target storage format for the
#'   `format` argument of [tar_target()] or [tar_option_set()].
#' @return A character string of length 1 encoding the custom format.
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
#' @section Format functions:
#'   In `tar_format()`, functions like `read`, `write`,
#'   `marshal`, and `unmarshal` must be perfectly pure
#'   and perfectly self-sufficient.
#'   They must load or namespace all their own packages,
#'   and they must not depend on any custom user-defined
#'   functions or objects in the global environment of your pipeline.
#'   `targets` converts each function to and from text,
#'   so it must not rely on any data in the closure.
#'   This disqualifies functions produced by `Vectorize()`,
#'   for example.
#' @param read A function with a single argument named `path`.
#'   This function should read and return the target stored
#'   at the file in the argument. It should have no side effects.
#'   See the "Format functions" section for specific requirements.
#' @param write A function with two arguments: `object` and `path`,
#'   in that order. This function should save the R object `object`
#'   to the file path at `path` and have no other side effects.
#'   The return value does not matter.
#'   See the "Format functions" section for specific requirements.
#' @param marshal A function with a single argument named `object`.
#'   This function should marshal the R object and return
#'   an in-memory object that can be exported to remote parallel workers.
#'   It should not read or write any persistent files.
#'   See the Marshalling section for details.
#'   See the "Format functions" section for specific requirements.
#' @param unmarshal A function with a single argument named `object`.
#'   This function should unmarshal the (marshalled) R object and return
#'   an in-memory object that is appropriate and valid for use
#'   on a parallel worker. It should not read or write any persistent files.
#'   See the Marshalling section for details.
#'   See the "Format functions" section for specific requirements.
#' @param repository Deprecated. Use the `repository` argument of
#'   [tar_target()] or [tar_option_set()] instead.
#' @examples
#' # The following target is equivalent to
#' # tar_target(name, command(), format = "keras"):
#' tar_target(
#'   name,
#'   command(),
#'   format = tar_format(
#'     read = function(path) {
#'        keras::load_model_hdf5(path)
#'     },
#'     write = function(object, path) {
#'       keras::save_model_hdf5(object = object, filepath = path)
#'     },
#'     marshal = function(object) {
#'       keras::serialize_model(object)
#'     },
#'     unmarshal = function(object) {
#'       keras::unserialize_model(object)
#'     }
#'   )
#' )
tar_format <- function(
  read = function(path) {
    readRDS(path) # nocov
  },
  write = function(object, path) {
    saveRDS(object = object, file = path, version = 3L) # nocov
  },
  marshal = function(object) {
    identity(object) # nocov
  },
  unmarshal = function(object) {
    identity(object) # nocov
  },
  repository = NULL
) {
  tar_assert_function(read)
  tar_assert_function(write)
  tar_assert_function(marshal)
  tar_assert_function(unmarshal)
  tar_assert_function_arguments(read, "path")
  tar_assert_function_arguments(write, c("object", "path"))
  tar_assert_function_arguments(marshal, "object")
  tar_assert_function_arguments(unmarshal, "object")
  if (!is.null(repository)) {
    tar_warn_deprecate(
      "in targets version > 0.10.0 (2022-03-13) the repository ",
      "argument of tar_format() is deprecated. Please use the ",
      "repository argument of tar_target() or tar_option_set() ",
      "instead."
    )
  }
  paste(
    "format_custom",
    tar_format_field("read", read),
    tar_format_field("write", write),
    tar_format_field("marshal", marshal),
    tar_format_field("unmarshal", unmarshal),
    paste0("repository=", repository),
    sep = "&"
  )
}

tar_format_field <- function(key, value) {
  paste0(key, "=", base64url::base64_urlencode(tar_deparse_safe(value)))
}
