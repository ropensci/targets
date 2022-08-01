#' @title Target resources: `clustermq` high-performance computing
#' @export
#' @family resources
#' @description Create the `clustermq` argument of `tar_resources()`
#'   to specify optional high-performance computing settings
#'   for `tar_make_clustermq()`.
#'   For details, see the documentation of the `clustermq` R package
#'   and the corresponding argument names in this help file.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_clustermq"`, to be supplied
#'   to the `clustermq` argument of `tar_resources()`.
#' @param template Named list, `template` argument to
#'   `clustermq::workers()`. Defaults to an empty list.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   resources = tar_resources(
#'     clustermq = tar_resources_clustermq(template = list(n_cores = 2))
#'   )
#' )
tar_resources_clustermq <- function(
  template = targets::tar_option_get("resources")$clustermq$template
) {
  template <- template %|||% list()
  out <- resources_clustermq_init(
    template = template
  )
  resources_validate(out)
  out
}
