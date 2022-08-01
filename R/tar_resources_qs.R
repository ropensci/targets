#' @title Target resources: qs storage formats
#' @export
#' @family resources
#' @description Create the `qs` argument of `tar_resources()`
#'   to specify optional settings for big data storage formats
#'   powered by the `qs` R package.
#'   See the `format` argument of [tar_target()] for details.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_qs"`, to be supplied
#'   to the qs argument of `tar_resources()`.
#' @param preset Character of length 1, `preset`
#'   argument of `qs::qsave()`. Defaults to `"high"`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   resources = tar_resources(
#'     qs = tar_resources_qs(preset = "fast")
#'   )
#' )
tar_resources_qs <- function(
  preset = targets::tar_option_get("resources")$qs$preset
) {
  preset <- preset %|||% "high"
  out <- resources_qs_init(
    preset = preset
  )
  resources_validate(out)
  out
}
