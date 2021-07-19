#' @title Target resources: `future` high-performance computing
#' @export
#' @family resources
#' @description Create the `future` argument of `tar_resources()`
#'   to specify optional high-performance computing settings
#'   for `tar_make_future()`.
#'   This is how to supply the `resources`
#'   argument of `future::future()` for `targets`.
#'   Resources supplied through
#'   `future::plan()` and `future::tweak()` are completely ignored.
#'   For details, see the documentation of the `future` R package
#'   and the corresponding argument names in this help file.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_future"`, to be supplied
#'   to the `future` argument of `tar_resources()`.
#' @param plan A `future::plan()` object or `NULL`,
#'   a `target`-specific `future` plan.
#' @param resources Named list, `resources` argument to
#'   `future::future()`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   resources = tar_resources(
#'     future = tar_resources_future(resources = list(n_cores = 2))
#'   )
#' )
tar_resources_future <- function(
  plan = NULL,
  resources = list()
) {
  out <- resources_future_init(
    plan = plan,
    resources = resources
  )
  resources_validate(out)
  out
}
