#' @title Unset one or more target options.
#' @export
#' @family configuration
#' @description Unset one or more target options you previously chose with
#'   [tar_option_set()]. These options are mostly configurable default
#'   arguments to [tar_target()] and [tar_target_raw()].
#' @return `NULL` (invisibly).
#' @param options A character vector of options to reset.
#'   Must all be elements of `names(formals(tar_option_set))`
#' @examples
#' tar_option_get("format") # default format before we set anything
#' tar_target(x, 1)$settings$format
#' tar_option_set(format = "fst_tbl") # new default format
#' tar_option_get("format")
#' tar_target(x, 1)$settings$format
#' tar_option_unset("format") # Unsets the format option.
#' tar_target(x, 1)$settings$format
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(cue = tar_cue(mode = "always"))
#'   tar_option_unset("cue") # Unsets the cue option.
#'   list(tar_target(x, 1), tar_target(y, 2))
#' })
#' tar_make()
#' tar_make()
#' })
#' }
tar_option_unset <- function(options) {
  tar_assert_in(x = options, choices = names(formals(tar_option_set)))
  for (option in options) {
    tar_options[[option]] <- NULL
  }
  invisible()
}
