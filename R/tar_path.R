#' @title Get the file path of a target's stored value.
#' @export
#' @description Get the file path of a target's stored value. Could be the
#'   target currently running in a pipeline or a target of your choice.
#' @details Call `tar_path(name = "your_target")` to get the file path
#'   of a selected target. Inside the command of a target, call
#'   `tar_path()` with no arguments to get the path to the target
#'   currently running. This function does not apply to dynamic files
#'   (targets with `format = "file"`)
#'   because those paths are not known in advance.
#' @return File path to the target.
#' @param name Symbol, name of the target.
#'   If `NULL`, `tar_path()` returns the path of the target currently running
#'   in a pipeline.
#' @examples
#' tar_path(your_target)
#' tar_target(returns_path, tar_path())
tar_path <- function(name = NULL) {
  name <- deparse_language(substitute(name))
  assert_chr(name %||% character(0), "name arg of tar_path() must be a symbol")
  trn(
    is.null(name),
    tar_path_running(),
    path_default(name)
  )
}

tar_path_running <- function(name) {
  trn(
    exists(x = "name", envir = envir_run, inherits = FALSE),
    path_default(get(x = "name", envir = envir_run)),
    throw_validate("tar_path() with no args only works inside a pipeline.")
  )
}
