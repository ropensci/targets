#' @rdname tar_timestamp
#' @export
tar_timestamp_raw <- function(
  name = NULL,
  format = NULL,
  tz = NULL,
  parse = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_chr(name %|||% character(0), "name must be a character.")
  if (!is.null(format)) {
    tar_warn_deprecate(
      "the format argument of tar_timestamp() was deprecated ",
      "in targets version 0.6.0 (2021-07-21)."
    )
  }
  if (!is.null(tz)) {
    tar_warn_deprecate(
      "the tz argument of tar_timestamp() was deprecated",
      "in targets version 0.6.0 (2021-07-21)."
    )
  }
  if (!is.null(parse)) {
    tar_warn_deprecate(
      "the parse argument of tar_timestamp() was deprecated",
      "in targets version 0.6.0 (2021-07-21)."
    )
  }
  if (is.null(name)) {
    if (is.null(tar_runtime$target)) {
      tar_throw_validate(
        "name cannot be NULL unless tar_timestamp() is called from a target."
      )
    }
    name <- target_get_name(tar_runtime$target)
  }
  tar_message_meta(store = store)
  meta <- meta_init(path_store = store)
  meta$preprocess(write = FALSE)
  if (!meta$exists_record(name)) {
    return(file_time_system_tz(file_time_reference))
  }
  record <- meta$get_record(name)
  time <- file_time_posixct(record$time)
  file_time_system_tz(time %||NA% file_time_reference)
}
