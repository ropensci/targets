#' @export
store_class_format.null <- function(format) {
  store_class_format_null
}

store_class_format_null <- c("tar_null", "tar_store")

#' @export
store_assert_format_setting.null <- function(format) {
  tar_throw_validate("Users should not set format = \"null\".")
}

#' @export
store_read_path.tar_null <- function(store, path) {
  NULL
}

#' @export
store_write_path.tar_null <- function(store, object, path) {
}

store_reformat_null <- function(target) {
  outcome <- metrics_outcome(target$metrics)
  error <- target$settings$error
  if (identical(outcome, "error") && identical(error, "null")) {
    target_reformat(target, "null")
  }
}
