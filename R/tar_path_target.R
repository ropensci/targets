#' @title Identify the file path where the current target will be stored.
#' @export
#' @family utilities
#' @description Identify the file path where the current target will be stored
#'   locally on disk.
#'   Designed to be called inside the command of a target currently
#'   running in a pipeline.
#'   See the "Value" section for specifics because the return
#'   value depends on the `format` and `repository` settings.
#' @return Character string, the file path where the target currently running
#'   will be stored. This path is not always known or available in advance,
#'   it depends on the `format` and `repository` settings.
#'
#'   If `format` is not `"file"` and `repository` is `"local"` (default)
#'   then `tar_path_target()` returns `"STORE/objects/YOUR_TARGET"`,
#'   where `STORE` is the value of `tar_config_get("store")` and
#'   `YOUR_TARGET` is the name of your target.
#'
#'   Otherwise, if `format` is `"file"`, then the return value is
#'   `NA_character_` because `targets` does not control where
#'   those files are stored.
#'
#'   Otherwise, if `format` is not `"file"` and `repository` is
#'   not `"local"`, then `tar_path_target()` returns
#'   the temporary staging path where the data is stored
#'   before it is uploaded to a remote repository.
#'   Remote repositories vary so widely that the eventual final location
#'   cannot always be known, but all remote repositories use staging
#'   files that `targets` knows about.
#'
#'   If not called from inside a running target,
#'   `tar_path_target(name = your_target)` just returns
#'   `"STORE/objects/your_target"`.
#' @param name Symbol, name of a target.
#'   If `NULL`, `tar_path_target()` returns the path of the target currently
#'   running in a pipeline.
#' @param default Character, value to return if `tar_path_target()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @param create_dir Logical of length 1,
#'   whether to create `dirname(tar_path_target())` in
#'   `tar_path_target()` itself.
#'   This is useful if you are writing to `tar_path_target()` from inside a
#'   `storage = "none"` target and need the parent directory of the file
#'   to exist.
#' @param store Character of length 1,
#'   path to the data store if `tar_path_target()`
#'   is called outside a running pipeline. If `tar_path_target()` is called
#'   inside a running pipeline, this argument is ignored
#'   and actual the path to the running pipeline's data store
#'   is used instead.
#' @examples
#' tar_path_target()
#' tar_path_target(your_target)
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target(returns_path, tar_path_target()), ask = FALSE)
#' tar_make()
#' tar_read(returns_path)
#' })
#' }
tar_path_target <- function(
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
  if (is.null(tar_runtime$target)) {
    return(as.character(default))
  }
  if (identical(tar_runtime$target$settings$format, "file")) {
    return(NA_character_)
  }
  store_tar_path(
    tar_runtime$target$store,
    tar_runtime$target,
    tar_runtime$store
  )
}
