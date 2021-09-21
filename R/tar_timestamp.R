#' @title Get the timestamp(s) of a target.
#' @export
#' @family time
#' @description Get the timestamp associated with a target's
#'   last successful run.
#' @details `tar_timestamp()` checks the metadata in `_targets/meta/meta`,
#'   not the actual returned data of the target.
#'   The timestamp depends on the storage format of the target.
#'   If storage is local, e.g. formats like `"rds"` and `"file"`,
#'   then the time stamp is the latest modification time
#'   of the target data files at the time the target
#'   last successfully ran. For non-local formats like
#'   `"aws_rds"` and `"url"`, then `targets` chooses instead
#'   to simply record the time the target last successfully ran.
#' @return If the target is not recorded in the metadata
#'   or cannot be parsed correctly, then
#'   `tar_timestamp()` returns a `POSIXct` object at `1970-01-01 UTC`.
#' @inheritParams tar_timestamp_raw
#' @param name Symbol, name of the target. If `NULL` (default)
#'   then `tar_timestamp()` will attempt to return the timestamp
#'   of the target currently running. Must be called inside a target's
#'   command or a supporting function in order to work.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
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
  format = NULL,
  tz = NULL,
  parse = NULL,
  store = targets::tar_config_get("store")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name %|||% character(0), "name must be a symbol.")
  tar_timestamp_raw(
    name = name,
    format = format,
    tz = tz,
    parse = parse,
    store = store
  )
}
