#' @export
store_class_format.auto <- function(format) {
  store_class_format_auto
}

store_class_format_auto <- c("tar_auto", "tar_store")

#' @export
store_assert_format_setting.auto <- function(format) {
}

#' @export
store_get_packages.tar_auto <- function(store) {
  "qs"
}

store_reformat_auto <- function(target) {
  if (!identical(target$settings$format, "auto")) {
    return()
  }
  object <- target$value$object
  format <- if_any(
    is.character(object) && all(file.exists(object)),
    "file",
    "qs"
  )
  target_reformat(target, format)
}
