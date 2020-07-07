#' @title Get the name of the target currently running.
#' @export
#' @description Get the name of the target currently running.
#' @details Must be invoked somewhere in the target's command.
#'   Only works inside a `targets` pipeline.
#' @examples
#' tar_target(returns_name, tar_name())
tar_name <- function() {
  if (!exists(x = "name", envir = envir_run, inherits = FALSE)) {
    throw_validate("tar_name() only works inside a targets pipeline.")
  }
  get(x = "name", envir = envir_run)
}
