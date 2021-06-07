#' @title Target resources: `clustermq` high-performance computing
#' @export
#' @family resources
#' @description Create the `clustermq` argument of [tar_resources()]]
#'   to specify optional high-performance computing settings
#'   for [tar_make_clustermq()].
#'   For details, see the documentation of the `clustermq` R package
#'   and the corresponding argument names in this help file.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_clustermq"`, to be supplied
#'   to the `clustermq` argument of [tar_resources()].
#' @param log_worker Logical of length 1, `log_worker`
#'   argument to `clustermq::workers()`.
#' @param template Named list, `template` argument to
#'   `clustermq::workers()`.
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
  log_worker = FALSE,
  template = list()
) {
  out <- resources_clustermq_init(
    log_worker = log_worker,
    template = template
  )
  resources_validate(out)
  out
}