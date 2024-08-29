#' @title Current storage format.
#' @export
#' @family utilities
#' @description Get the storage format of the target currently running.
#' @details This function is meant to be called inside a target in a
#'   running pipeline. If it is called outside a target in the running
#'   pipeline, it will return the default format given by
#'   `tar_option_get("format")`.
#' @return A character string, storage format of the target currently
#'   running in the pipeline. If called outside a target in the running
#'   pipeline, [tar_format_get()] will return the default format given by
#'   `tar_option_get("format")`.
#' @examples
#' tar_target(x, tar_format_get(), format = "qs")
tar_format_get <- function() {
  tar_definition()$settings$format
}
