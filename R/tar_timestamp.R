#' @title Get the timestamp of a target.
#' @export
#' @family utilities
#' @description Get the time that a target's data was last modified.
#' @details If the files are stored locally, the time
#' @return A POSIXct object. If the target does not exists,
#'   the return value is `as.POSIXct("0000-01-01")`.
#'   If the target has a local storage
#'   format, the return value is the maximum timestamp
#'   over all the files. (Targets with `format = "file"`
#'   may have multiple files.) For AWS-backed formats
#'   such as `"aws_parquet"`, the time stamp is obtained
#'   from the modification timestamp of the AWS S3 object header.
#' @param name Symbol, name of the target. If `NULL` (default)
#'   then `tar_timestamp()` will attempt to return the timestamp
#'   of the target currently running. Must be called inside a target's
#'   command or a supporting function in order to work.
#' @param format Character of length 1, POSIXct format string passed to
#'   `strptime()` to parse the time stamp of a URL or AWS S3 bucket.
#'   Currently to targets with AWS-backed storage
#'   formats or `format = "url"`. The default works with AWS S3
#'   buckets and <https://httpbin.org> but may not work for all URLs.
#'   Outside `targets`, you can use the `curl` package or the `curl` utility
#'   to get time stamps of URLs.
#' @param tz Character of length 1, time zone passed to
#'   `strptime()` to parse the time stamp of a URL or AWS S3 bucket.
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
  tz = "GMT"
) {
  name <- deparse_language(substitute(name))
  assert_chr(name %|||% character(0), "name must be a symbol")
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
    return(tar_timestamp_default)
  }
  record <- meta$get_record(name)
  store <- store_init(format = record$format)
  store$file$path <- record$path
  store_get_timestamp(store = store, format = format, tz = tz) %||NA%
    tar_timestamp_default
}

tar_timestamp_default <- as.POSIXct("0000-01-01")
