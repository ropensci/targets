#' @title Get the timestamp(s) of a target (raw version).
#' @export
#' @family utilities
#' @description Get the time that a target's data was last modified.
#'   If there are multiple artifacts, as with file or URL targets,
#'   then multiple time stamps may be returned.
#' @details `tar_timestamp_raw()` is like `tar_timestamp()` except
#'   it accepts a character string for `name` instead of a symbol.
#'   `tar_timestamp_raw()` checks the actual data,
#'   not the metadata, so the returned time stamps
#'   are more up-to-date than the ones from [tar_meta()].
#' @return If the target is not recorded in the metadata
#'   or cannot be parsed correctly, then
#'   `tar_timestamp_raw()` returns a `POSIXct` object at `1970-01-01 UTC`.
#'   If the target is recorded in the metadata and stored locally
#'   (i.e. the target is not a URL or AWS-backed format) then `tar_timestamp()`
#'   returns a vector of `POSIXct` modification times of the data files
#'   (in the order those files are listed in `tar_meta(target_name, path)`).
#'   The return values for URL and AWS targets depends on the `parse` argument.
#'   If `parse` is `FALSE`, then `tar_timestamp()` returns an unparsed
#'   character vector of the time stamps recorded on the web.
#'   If `parse` is `TRUE`, then `tar_timestamp()` attempts to convert
#'   those character time stamps into `POSIXct` objects displayed
#'   with the time zone of the current system. If the time stamp
#'   cannot be parsed with the given format, `tar_timestamp()`
#'   returns a `POSIXct` object at `1970-01-01 UTC`
#'   (so `parse = FALSE` is helpful for debugging
#'   the `format` argument).
#' @param name Character of length 1, name of the target.
#' @param format Character of length 1, POSIXct format string passed to
#'   `as.POSIXct()` to parse the time stamp of a URL or AWS S3 bucket.
#'   Currently to targets with AWS-backed storage
#'   formats or `format = "url"`. The default works with AWS S3
#'   buckets but may not work for all URLs.
#' @param tz Character of length 1, time zone of the original
#'   modification time recorded in the remote data
#'   (either a URL or S3 bucket; does not apply to locally stored targets).
#'   The `tz` argument is passed to
#'   `as.POSIXct()` to parse the time stamp of a URL or AWS S3 bucket.
#'   The time stamp of the return value
#'   is the time zone of the system, not the time zone
#'   originally recorded in the time stamp.
#' @param parse Logical, whether to attempt to parse character string
#'   time stamps from URLs and data in AWS S3 buckets. Set to `FALSE`
#'   to debug. Debugging is important because incorrectly parsed
#'   time stamps will result in a return value of
#'   a `POSIXct` object at `1970-01-01 UTC` instead of `NA`.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
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
  format = "%a, %d %b %Y %H:%M:%S",
  tz = "UTC",
  parse = TRUE
) {
  assert_chr(name %|||% character(0), "name must be a character.")
  if (is.null(name)) {
    if (!exists(x = "target", envir = tar_envir_run, inherits = FALSE)) {
      throw_validate(
        "name cannot be NULL unless tar_timestamp() is called from a target."
      )
    }
    name <- target_get_name(get(x = "target", envir = tar_envir_run))
  }
  meta <- meta_init()
  meta$database$preprocess(write = FALSE)
  if (!meta$exists_record(name)) {
    return(file_time_system_tz(file_time_reference))
  }
  record <- meta$get_record(name)
  store <- store_init(format = record$format)
  store$file$path <- record$path
  out <- store_get_timestamp(store = store)
  # Tested in tests/interactive/test-class_url.R
  # nocov start
  if (is.character(out) && parse) {
    out <- file_time_system_tz(as.POSIXct(out, format = format, tz = tz))
  }
  # nocov end
  out %||NA% file_time_system_tz(file_time_reference)
}
