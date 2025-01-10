#' @title Define a custom target storage format.
#' @export
#' @family storage
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
#'   In [tar_format()], functions like `read`, `write`,
#'   `marshal`, and `unmarshal` must be perfectly pure
#'   and perfectly self-sufficient.
#'   They must load or namespace all their own packages,
#'   and they must not depend on any custom user-defined
#'   functions or objects in the global environment of your pipeline.
#'   `targets` converts each function to and from text,
#'   so it must not rely on any data in the closure.
#'   This disqualifies functions produced by `Vectorize()`,
#'   for example.
#'
#'   The `write` function must write only a single file,
#'   and the file it writes must not be a directory.
#'
#'   The functions to read and write the object
#'   should not do any conversions on the object. That is the job
#'   of the `convert` argument. The `convert` argument is a function
#'   that accepts the object returned by the command of the target
#'   and changes it into an acceptable format (e.g. can be
#'   saved with the `read` function). Working with the `convert`
#'   function is best because it ensures the in-memory copy
#'   of an object during the running pipeline session
#'   is the same as the copy of the object that is saved
#'   to disk.
#' @param read A function with a single argument named `path`.
#'   This function should read and return the target stored
#'   at the file in the argument. It should have no side effects.
#'   See the "Format functions" section for specific requirements.
#'   If `NULL`, the `read` argument defaults to `readRDS()`.
#' @param write A function with two arguments: `object` and `path`,
#'   in that order. This function should save the R object `object`
#'   to the file path at `path` and have no other side effects.
#'   The function need not return a value, but the file written to `path`
#'   must be a single file, and it cannot be a directory.
#'   See the "Format functions" section for specific requirements.
#'   If `NULL`, the `write` argument defaults to `saveRDS()`
#'   with `version = 3`.
#' @param marshal A function with a single argument named `object`.
#'   This function should marshal the R object and return
#'   an in-memory object that can be exported to remote parallel workers.
#'   It should not read or write any persistent files.
#'   See the Marshalling section for details.
#'   See the "Format functions" section for specific requirements.
#'   If `NULL`, the `marshal` argument defaults to just
#'   returning the original object without any modifications.
#' @param unmarshal A function with a single argument named `object`.
#'   This function should unmarshal the (marshalled) R object and return
#'   an in-memory object that is appropriate and valid for use
#'   on a parallel worker. It should not read or write any persistent files.
#'   See the Marshalling section for details.
#'   See the "Format functions" section for specific requirements.
#'   If `NULL`, the `unmarshal` argument defaults to just
#'   returning the original object without any modifications.
#' @param convert The `convert` argument is a function with a single argument
#'   named `object`.
#'   It accepts the object returned by the command of the target
#'   and changes it into an acceptable format (e.g. can be
#'   saved with the `read` function). The `convert`
#'   ensures the in-memory copy
#'   of an object during the running pipeline session
#'   is the same as the copy of the object that is saved
#'   to disk. The function should be idempotent, and it should
#'   handle edge cases like `NULL` values (especially for
#'   `error = "null"` in [tar_target()] or [tar_option_set()]).
#'   If `NULL`, the `convert` argument defaults to just
#'   returning the original object without any modifications.
#' @param copy The `copy` argument is a function with a single function
#'   named `object`.
#'   It accepts the object returned by the command of the target
#'   and makes a deep copy in memory. This method does is relevant
#'   to objects like `data.table`s that support in-place modification
#'   which could cause unpredictable side effects from target
#'   to target. In cases like these, the target should be deep-copied
#'   before a downstream target attempts to use it (in the case of
#'   `data.table` objects, using `data.table::copy()`).
#'   If `NULL`, the `copy` argument defaults to just
#'   returning the original object without any modifications.
#' @param substitute Named list of values to be inserted into the
#'   body of each custom function in place of symbols in the body.
#'   For example, if
#'   `write = function(object, path) saveRDS(object, path, version = VERSION)`
#'   and `substitute = list(VERSION = 3)`, then
#'   the `write` function will actually end up being
#'   `function(object, path) saveRDS(object, path, version = 3)`.
#'
#'   Please do not include temporary or sensitive information
#'   such as authentication credentials.
#'   If you do, then `targets` will write them
#'   to metadata on disk, and a malicious actor could
#'   steal and misuse them. Instead, pass sensitive information
#'   as environment variables using [tar_resources_custom_format()].
#'   These environment variables only exist in the transient memory
#'   spaces of the R sessions of the local and worker processes.
#' @param repository Deprecated. Use the `repository` argument of
#'   [tar_target()] or [tar_option_set()] instead.
#' @examples
#' # The following target is equivalent to the current superseded
#' # tar_target(name, command(), format = "keras").
#' # An improved version of this would supply a `convert` argument
#' # to handle NULL objects, which are returned by the target if it
#' # errors and the error argument of tar_target() is "null".
#' tar_target(
#'   name = keras_target,
#'   command = your_function(),
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
#'     }
#'   )
#' )
#' # And the following is equivalent to the current superseded
#' # tar_target(name, torch::torch_tensor(seq_len(4)), format = "torch"),
#' # except this version has a `convert` argument to handle
#' # cases when `NULL` is returned (e.g. if the target errors out
#' # and the `error` argument is "null" in tar_target()
#' # or tar_option_set())
#' tar_target(
#'   name = torch_target,
#'   command = torch::torch_tensor(),
#'   format = tar_format(
#'     read = function(path) {
#'       torch::torch_load(path)
#'     },
#'     write = function(object, path) {
#'       torch::torch_save(obj = object, path = path)
#'     },
#'     marshal = function(object) {
#'       con <- rawConnection(raw(), open = "wr")
#'       on.exit(close(con))
#'       torch::torch_save(object, con)
#'       rawConnectionValue(con)
#'     },
#'     unmarshal = function(object) {
#'       con <- rawConnection(object, open = "r")
#'       on.exit(close(con))
#'       torch::torch_load(con)
#'     }
#'   )
#' )
tar_format <- function(
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  convert = NULL,
  copy = NULL,
  substitute = list(),
  repository = NULL
) {
  if (!is.null(read)) {
    tar_assert_function(read)
    tar_assert_function_arguments(read, "path")
    read <- tar_sub_body(read, substitute)
  }
  if (!is.null(write)) {
    tar_assert_function(write)
    tar_assert_function_arguments(write, c("object", "path"))
    write <- tar_sub_body(write, substitute)
  }
  if (!is.null(marshal)) {
    tar_assert_function(marshal)
    tar_assert_function_arguments(marshal, "object")
    marshal <- tar_sub_body(marshal, substitute)
  }
  if (!is.null(unmarshal)) {
    tar_assert_function(unmarshal)
    tar_assert_function_arguments(unmarshal, "object")
    unmarshal <- tar_sub_body(unmarshal, substitute)
  }
  if (!is.null(convert)) {
    tar_assert_function(convert)
    tar_assert_function_arguments(convert, "object")
    convert <- tar_sub_body(convert, substitute)
  }
  if (!is.null(copy)) {
    tar_assert_function(copy)
    tar_assert_function_arguments(copy, "object")
    copy <- tar_sub_body(copy, substitute)
  }
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
    tar_format_field("convert", convert),
    tar_format_field("copy", copy),
    paste0("repository=", repository),
    sep = "&"
  )
}

tar_format_field <- function(key, value) {
  if (is.null(value)) {
    return(paste0(key, "="))
  }
  if (is.function(value)) {
    value <- body(value)
  }
  paste0(key, "=", base64url::base64_urlencode(tar_deparse_safe(value)))
}
