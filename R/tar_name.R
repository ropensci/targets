#' @title Get the name of the target currently running.
#' @export
#' @description Get the name of the target currently running.
#' @details If invoked inside the target's command.
#'   Only works inside a `targets` pipeline.
#' @return Character of length 1. If called inside a pipeline,
#'   `tar_name()` returns name of the target currently running.
#'   Otherwise, the return value is `default`.
#' @param default Character, value to return if `tar_name()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_name()
#' tar_name(default = "custom_target_name")
#' tar_target(name, tar_name(default = "custom")) # Returns "name".
tar_name <- function(default = "target") {
  trn(
    exists(x = "name", envir = tar_envir_run, inherits = FALSE),
    get(x = "name", envir = tar_envir_run),
    as.character(default)
  )
}
