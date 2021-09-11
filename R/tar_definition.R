#' @title For developers only: get the definition of the current target.
#' @export
#' @family utilities
#' @description For developers only: get the full definition of the
#'   target currently running. This target definition is the same kind
#'   of object produced by [tar_target()].
#' @details Most users should not use `tar_definition()`  because accidental
#'   modifications could break the pipeline.
#'   `tar_definition()` only exists in order to support third-party interface
#'   packages, and even then the returned target definition is not modified..
#' @return If called from a running target, `tar_definition()` returns
#'   the target object of the currently running target.
#'   See the "Target objects" section for details.
#' @inheritSection tar_target Target objects
#' @param default Environment, value to return if `tar_definition()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' class(tar_definition())
#' tar_definition()$settings$name
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(
#'   tar_target(x, tar_definition()$settings$memory, memory = "transient")
#' )
#' tar_make(x)
#' tar_read(x)
#' })
#' }
tar_definition <- function(
  default = targets::tar_target_raw("target_name", quote(identity()))
) {
  tar_assert_target(default)
  if_any(tar_runtime$exists_target(), tar_runtime$get_target(), default)
}
