#' @title List new targets
#' @export
#' @family time
#' @description List all the targets whose last successful run occurred
#'   after a certain point in time.
#' @details Only applies to targets with recorded time stamps:
#'   just non-branching targets and individual dynamic branches.
#'   As of `targets` version 0.6.0, these time
#'   stamps are available for these targets regardless of
#'   storage format. Earlier versions of `targets` do not record
#'   time stamps for remote storage such as `format = "url"`
#'   or `repository = "aws"` in [tar_target()].
#' @return A character vector of names of old targets with recorded
#'   timestamp metadata.
#' @inheritParams tar_older
#' @param names Names of eligible targets. Targets excluded from `names`
#'   will not be returned even if they are newer than the given `time`.
#'   You can supply symbols
#'   or `tidyselect` helpers like [any_of()] and [starts_with()].
#'   If `NULL`, all names are eligible.
#' @param time A `POSIXct` object of length 1, time threshold.
#'   Targets newer than this time stamp are returned.
#'   For example, if `time = Sys.time - as.difftime(1, units = "weeks")`
#'   then `tar_newer()` returns targets newer than one week ago.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   list(tar_target(x, seq_len(2)))
#' }, ask = FALSE)
#' tar_make()
#' # targets newer than 1 week ago
#' tar_newer(Sys.time() - as.difftime(1, units = "weeks"))
#' # targets newer than 1 week from now
#' tar_newer(Sys.time() + as.difftime(1, units = "weeks"))
#' # Everything is still up to date.
#' tar_make()
#' # Invalidate all targets targets newer than 1 week ago
#' # so they run on the next tar_make().
#' invalidate_these <- tar_newer(Sys.time() - as.difftime(1, units = "weeks"))
#' tar_invalidate(any_of(invalidate_these))
#' tar_make()
#' })
#' }
tar_newer <- function(
  time,
  names = NULL,
  inclusive = FALSE,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_newer")
  tar_assert_scalar(time)
  tar_assert_inherits(time, "POSIXct")
  tar_assert_scalar(inclusive)
  tar_assert_lgl(inclusive)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  meta <- meta_init(path_store = store)
  meta <- tibble::as_tibble(meta$database$read_condensed_data())
  tar_message_meta(meta)
  meta <- meta[!is.na(meta$time), ]
  meta$time <- file_time_posixct(meta$time)
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, meta$name) %|||% meta$name
  if_any(
    inclusive,
    meta$name[meta$name %in% names & meta$time >= time],
    meta$name[meta$name %in% names & meta$time > time]
  )
}
