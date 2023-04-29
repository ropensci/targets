#' @title Get the timestamp(s) of a target (raw version).
#' @export
#' @family time
#' @description Get the time that a target last ran successfully.
#' @details `tar_timestamp_raw()` is like `tar_timestamp()` except
#'   it accepts a character string for `name` instead of a symbol.
#'   `tar_timestamp_raw()` checks the metadata in `_targets/meta/meta`,
#'   not the actual data. Time stamps are recorded only for targets that
#'   run commands: just non-branching targets and individual dynamic
#'   branches.
#' @return If the target is not recorded in the metadata
#'   or cannot be parsed correctly, then
#'   `tar_timestamp_raw()` returns a `POSIXct` object at `1970-01-01 UTC`.
#' @inheritParams tar_validate
#' @param name Character of length 1, name of the target.
#' @param format Deprecated in `targets` version 0.6.0 (2021-07-21).
#' @param tz Deprecated in `targets` version 0.6.0 (2021-07-21).
#' @param parse Deprecated in `targets` version 0.6.0 (2021-07-21).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(tar_target(x, 1))
#' }, ask = FALSE)
#' tar_make()
#' # Get the timestamp.
#' tar_timestamp_raw("x")
#' # We can use the timestamp to cancel the target
#' # if it already ran within the last hour.
#' # Be sure to set `cue = tar_cue(mode = "always")`
#' # if you want the target to always check the timestamp.
#' tar_script({
#'   list(
#'   tar_target(
#'     x,
#'     tar_cancel((Sys.time() - tar_timestamp_raw()) < 3600),
#'     cue = tar_cue(mode = "always")
#'   )
#' )}, ask = FALSE)
#' tar_make()
#' })
#' }
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
  meta <- meta_init(path_store = store)
  meta$database$preprocess(write = FALSE)
  if (!meta$exists_record(name)) {
    return(file_time_system_tz(file_time_reference))
  }
  record <- meta$get_record(name)
  time <- file_time_posixct(record$time)
  file_time_system_tz(time %||NA% file_time_reference)
}
