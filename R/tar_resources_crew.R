#' @title Target resources: `crew` high-performance computing
#' @export
#' @family resources
#' @description Create the `crew` argument of `tar_resources()`
#'   to specify optional target settings.
#' @details `tar_resources_crew()` accepts
#'   target-specific settings for integration with the
#'   `crew` R package. These settings are arguments to the `push()`
#'   method of the controller or controller group
#'   object which control things like
#'   auto-scaling behavior and the controller to use in the case
#'   of a controller group.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_crew"`, to be supplied
#'   to the `crew` argument of `tar_resources()`.
#' @param controller Character of length 1.
#'   If `tar_option_get("controller")` is a
#'   `crew` controller group, the `controller` argument of
#'   `tar_resources_crew()` indicates which controller in the controller
#'   group to use. If you need heterogeneous workers,
#'   you can leverage this argument to send different
#'   targets to different worker groups.
#' @param scale Deprecated in `targets` version 1.3.0.9002 (2023-10-02).
#'   No longer necessary.
#' @param seconds_timeout Positive numeric of length 1,
#'   optional task timeout passed to the `.timeout`
#'   argument of `mirai::mirai()` (after converting to milliseconds).
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   resources = tar_resources(
#'     crew = tar_resources_crew(seconds_timeout = 5)
#'   )
#' )
tar_resources_crew <- function(
  controller = targets::tar_option_get("resources")$crew$controller,
  scale = NULL,
  seconds_timeout = targets::tar_option_get("resources")$crew$seconds_timeout
) {
  if (!is.null(scale)) {
    tar_warn_deprecate(
      "The scale argument of tar_resources_crew() is ",
      "obsolete and deprecated (targets version 1.3.0.9002, 2023-10-02)."
    )
  }
  out <- resources_crew_init(
    controller = controller,
    seconds_timeout = seconds_timeout
  )
  resources_validate(out)
  out
}
