#' @title Deprecated: get the environment of the current target.
#' @export
#' @keywords internal
#' @description Deprecated in `targets` version 0.3.0 (2020-03-24).
#'   Gets the parent frame. In earlier versions,
#'   `tar_envir()` could return the environment where a
#'   target runs its command (inherits from `tar_option_get("envir")`).
#'   Recent infrastructure changes make this no longer possible.
#' @details Users should not call `tar_envir()` directly because accidental
#'   modifications to `parent.env(tar_envir())` could break the pipeline.
#'   `tar_envir()` only exists in order to support third-party interface
#'   packages such as `tarchetypes`.
#' @return An environment, just the parent frame.
#'   In `targets` <= 0.2.0, `tar_envir()` returned the top-level
#'   environment where the target was running. Recent
#'   infrastructure changes make this no longer possible.
#' @param default Environment, value to return if `tar_envir()`
#'   is called on its own outside a `targets` pipeline.
#'   Since `targets >= 0.3.0`, the default is always the return value.
#' @examples
#' tar_envir()
tar_envir <- function(default = parent.frame()) {
  assert_envir(default)
  warn_deprecate("tar_envir() is deprecated in targets >= 0.3.0")
  default
}
