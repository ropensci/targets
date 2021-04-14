#' @title Get the timestamp(s) of a target.
#' @export
#' @family utilities
#' @description Get the time that a target's data was last modified.
#'   If there are multiple artifacts, as with file or URL targets,
#'   then multiple time stamps may be returned.
#' @details `tar_timestamp()` checks the actual data,
#'   not the metadata, so the returned time stamps
#'   are more up-to-date than the ones from [tar_meta()].
#' @return If the target is not recorded in the metadata
#'   or cannot be parsed correctly, then
#'   `tar_timestamp()` returns a `POSIXct` object at `1970-01-01 UTC`.
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
#' @inheritParams tar_timestamp_raw
#' @param name Symbol, name of the target. If `NULL` (default)
#'   then `tar_timestamp()` will attempt to return the timestamp
#'   of the target currently running. Must be called inside a target's
#'   command or a supporting function in order to work.
#' @examples
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(tar_target(x, 1))
#' }, ask = FALSE)
#' tar_make()
#' # Get the timestamp.
#' tar_timestamp(x)
#' # We can use the timestamp to cancel the target
#' # if it already ran within the last hour.
#' # Be sure to set `cue = tar_cue(mode = "always")`
#' # if you want the target to always check the timestamp.
#' tar_script({
#'   list(
#'   tar_target(
#'     x,
#'     tar_cancel((Sys.time() - tar_timestamp()) < 3600),
#'     cue = tar_cue(mode = "always")
#'   )
#' )}, ask = FALSE)
#' tar_make()
#' })
#' }
tar_timestamp <- function(
  name = NULL,
  format = "%a, %d %b %Y %H:%M:%S",
  tz = "UTC",
  parse = TRUE
) {
  name <- deparse_language(substitute(name))
  assert_chr(name %|||% character(0), "name must be a symbol.")
  tar_timestamp_raw(name = name, format = format, tz = tz, parse = parse)
}
