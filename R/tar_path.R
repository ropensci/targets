#' @title Identify the file path where a target will be stored.
#' @export
#' @family utilities
#' @description Identify the file path where a target will be stored
#'   after the target finishes running in the pipeline.
#' @return Character, file path of the return value of the target.
#'   If not called from inside a running target,
#'   `tar_path(name = your_target)` just returns
#'   `_targets/objects/your_target`, the file path where `your_target`
#'   will be saved unless `format` is equal to `"file"` or any of the
#'   supported cloud-based storage formats.
#'
#'   For non-cloud storage formats, if you call `tar_path()`
#'   with no arguments while target `x` is running, the `name`
#'   argument defaults to the name of the running target,
#'   so `tar_path()` returns `_targets/objects/x`.
#'
#'   For cloud-backed formats, `tar_path()` returns the
#'   path to the staging file in `_targets/scratch/`.
#'   That way, even if you select a cloud repository
#'   (e.g. `tar_target(..., repository = "aws", storage = "none")`)
#'   then you can still manually write to `tar_path(create_dir = TRUE)`
#'   and the `targets` package will automatically hash it and
#'   upload it to the AWS S3 bucket. This does not apply to
#'   `format = "file"`, where you would never need `storage = "none"`
#'   anyway.
#' @param name Symbol, name of a target.
#'   If `NULL`, `tar_path()` returns the path of the target currently running
#'   in a pipeline.
#' @param default Character, value to return if `tar_path()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @param create_dir Logical of length 1,
#'   whether to create `dirname(tar_path())` in `tar_path()` itself.
#'   This is useful if you are writing to `tar_path()` from inside a
#'   `storage = "none"` target and need the parent directory of the file
#'   to exist.
#' @param store Character of length 1, path to the data store if `tar_path()`
#'   is called outside a running pipeline. If `tar_path()` is called
#'   inside a running pipeline, this argument is ignored
#'   and actual the path to the running pipeline's data store
#'   is used instead.
#' @examples
#' tar_path()
#' tar_path(your_target)
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(tar_target(returns_path, tar_path()), ask = FALSE)
#' tar_make()
#' tar_read(returns_path)
#' })
#' }
tar_path <- function(
  name = NULL,
  default = NA_character_,
  create_dir = FALSE,
  store = targets::tar_config_get("store")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name %|||% character(0))
  tar_assert_chr(default)
  tar_assert_lgl(create_dir)
  tar_assert_scalar(create_dir)
  out <- if_any(
    is.null(name),
    tar_path_running(default, path_store = store),
    path_objects(path_store = store, name = name)
  )
  if (create_dir) {
    dir_create(dirname(out))
  }
  out
}

tar_path_running <- function(default, path_store) {
  if_any(
    tar_runtime$exists_target(),
    store_tar_path(
      tar_runtime$get_target()$store,
      tar_runtime$get_target(),
      tar_runtime$get_store()
    ),
    as.character(default)
  )
}
