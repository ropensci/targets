#' @title Get the file path of a target's stored value.
#' @export
#' @description Get the file path of a target's stored value. Could be the
#'   target currently running in a pipeline or a target of your choice.
#' @details Call `tar_path(name = "your_target")` to get the file path
#'   of a selected target. Inside the command of a target, call
#'   `tar_path()` with no arguments to get the path to the target
#'   currently running. (Does not apply to dynamic files
#'   (`format = "file"`) because those paths are not known in advance.)
#'   If you call `tar_path()` outside a pipeline without specifying `name`,
#'   the return value is `default`.
#' @return Character, file path to the target.
#' @param name Symbol, name of the target.
#'   If `NULL`, `tar_path()` returns the path of the target currently running
#'   in a pipeline.
#' @param default Character, value to return if `tar_path()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_path()
#' tar_path(your_target)
#' tar_target(returns_path, tar_path())
tar_path <- function(name = NULL, default = tempfile()) {
  name <- deparse_language(substitute(name))
  assert_chr(name %||% character(0), "name arg of tar_path() must be a symbol")
  trn(
    is.null(name),
    tar_path_running(default),
    path_objects(name)
  )
}

tar_path_running <- function(default) {
  trn(
    exists(x = "target", envir = tar_envir_run, inherits = FALSE),
    path_objects(target_get_name(get(x = "target", envir = tar_envir_run))),
    as.character(default)
  )
}
