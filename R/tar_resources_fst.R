#' @title Target resources: `fst` storage formats
#' @export
#' @family resources
#' @description Create the `fst` argument of `tar_resources()`
#'   to specify optional settings for big data frame storage formats
#'   powered by the `fst` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_fst"`, to be supplied
#'   to the `fst` argument of `tar_resources()`.
#' @param compress Numeric of length 1, `compress`
#'   argument of `fst::write_fst()`. Defaults to `50`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "fst_tbl",
#'   resources = tar_resources(
#'     fst = tar_resources_fst(compress = 100)
#'   )
#' )
tar_resources_fst <- function(
  compress = targets::tar_option_get("resources")$fst$compress
) {
  compress <- compress %|||% 50
  out <- resources_fst_init(
    compress = compress
  )
  resources_validate(out)
  out
}
