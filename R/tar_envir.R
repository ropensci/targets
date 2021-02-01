#' @title For developers only: get the environment of the current target.
#' @export
#' @description For developers only: get the environment where a
#'   target runs its command. Inherits from `tar_option_get("envir")`.
#' @details Users should not call `tar_envir()` directly because accidental
#'   modifications to `parent.env(tar_envir())` could break the pipeline.
#'   `tar_envir()` only exists in order to support third-party interface
#'   packages such as `tarchetypes`.
#' @return If called from a running target, `tar_envir()` returns
#'   the environment where the target runs its command.
#'   If called outside a pipeline, the return value is
#'   whatever the user supplies to `default`
#'   (which defaults to `parent.frame()`).
#' @param default Environment, value to return if `tar_envir()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_envir()
#' tar_envir(default = new.env(parent = emptyenv()))
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(tar_target(x, tar_envir(default = parent.frame())))
#' tar_make(x)
#' tar_read(x)
#' })
#' }
tar_envir <- function(default = parent.frame()) {
  assert_envir(default)
  trn(
    exists(x = "target", envir = tar_envir_run, inherits = FALSE),
    get(x = "target", envir = tar_envir_run)$cache$targets$envir,
    default
  )
}
