#' @title Identify the file path where a target will be stored.
#' @export
#' @family utilities
#' @description Identify the file path where a target will be stored
#'   after the target finishes running in the pipeline.
#' @details `tar_path(name = your_target)` just returns
#'   `_targets/objects/your_target`, the file path where `your_target`
#'   will be saved unless `format` is equal to `"file"` or any of the
#'   supported cloud-based storage formats. If you call `tar_path()`
#'   with no arguments while target `x` is running, the `name`
#'   argument defaults to the name of the target,
#'   so `tar_path()` returns `_targets/objects/x`.
#' @return Character, file path to a hypothetical target.
#' @inheritParams tar_validate
#' @param name Symbol, name of a target.
#'   If `NULL`, `tar_path()` returns the path of the target currently running
#'   in a pipeline.
#' @param default Character, value to return if `tar_path()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
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
  store = targets::tar_config_get("store")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name %|||% character(0))
  tar_assert_chr(default)
  if_any(
    is.null(name),
    tar_path_running(default, path_store = store),
    path_objects(path_store = store, name = name)
  )
}

tar_path_running <- function(default, path_store) {
  if_any(
    tar_runtime$exists_target(),
    path_objects(
      path_store = path_store,
      name = target_get_name(tar_runtime$get_target())
    ),
    as.character(default)
  )
}
