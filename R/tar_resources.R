#' @title Target resources
#' @export
#' @family resources
#' @description Create a `resources` argument for [tar_target()]
#'   or [tar_option_set()].
#' @section Resources:
#'   Functions [tar_target()] and [tar_option_set()]
#'   each takes an optional `resources` argument to supply
#'   non-default settings of various optional backends for data storage
#'   and high-performance computing. The `tar_resources()` function
#'   is a helper to supply those settings in the correct manner.
#'   Resources are all-or-nothing: if you specify any resources
#'   with [tar_target()], all the resources from `tar_option_get("resources")`
#'   are dropped for that target. In other words, if you write
#'   `tar_option_set(resources = resources_1)` and then
#'   `tar_target(x, my_command(), resources = resources_2)`, then everything
#'   in `resources_1` is discarded for target `x`.
#' @return A list of objects of class `"tar_resources"` with
#'   non-default settings of various optional backends for data storage
#'   and high-performance computing.
#' @param aws Output of function `tar_resources_aws()`.
#'   AWS S3 storage settings for AWS backed storage formats
#'   such as `"aws_qs"` and `"aws_parquet`. Applies to all formats
#'   beginning with the `"aws_"` prefix. For details on formats,
#'   see the `format` argument of [tar_target()].
#' @param clustermq Output of function `tar_resources_clustermq()`.
#'   Optional `clustermq` settings for `tar_make_clustermq()`,
#'   including the `log_worker` and `template` arguments of
#'   `clustermq::workers()`.
#' @param feather Output of function `tar_resources_feather()`.
#'   Non-default arguments to `arrow::read_feather()` and
#'   `arrow::write_feather()` for `arrow`/feather-based storage formats.
#'   Applies to all formats ending with the `"_feather"` suffix.
#'   For details on formats, see the `format` argument of [tar_target()].
#' @param fst Output of function `tar_resources_fst()`.
#'   Non-default arguments to `fst::read_fst()` and
#'   `fst::write_fst()` for `fst`-based storage formats.
#'   Applies to all formats ending with `"fst"` in the name.
#'   For details on formats, see the `format` argument of [tar_target()].
#' @param future Output of function `tar_resources_future()`.
#'   Optional `future` settings for `tar_make_future()`,
#'   including the `resources` argument of
#'   `future::future()`, which can include values to insert in
#'   template placeholders in `future.batchtools` template files.
#'   This is how to supply the `resources`
#'   argument of `future::future()` for `targets`.
#'   Resources supplied through
#'   `future::plan()` and `future::tweak()` are completely ignored.
#' @param parquet Output of function `tar_resources_parquet()`.
#'   Non-default arguments to `arrow::read_parquet()` and
#'   `arrow::write_parquet()` for `arrow`/parquet-based storage formats.
#'   Applies to all formats ending with the `"_parquet"` suffix.
#'   For details on formats, see the `format` argument of [tar_target()].
#' @param qs Output of function `tar_resources_qs()`.
#'   Non-default arguments to `qs::qread()` and
#'   `qs::qsave()` for `qs`-based storage formats.
#'   Applies to all formats ending with the `"_qs"` suffix.
#'   For details on formats, see the `format` argument of [tar_target()].
#' @param url Output of function `tar_resources_url()`.
#'   Non-default settings for storage formats ending with the `"_url"` suffix.
#'   These settings include the `curl` handle for extra control over HTTP
#'   requests. For details on formats, see the `format` argument of
#'   [tar_target()].
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   resources = tar_resources(
#'     qs = tar_resources_qs(preset = "fast"),
#'     future = tar_resources_future(resources = list(n_cores = 1))
#'   )
#' )
tar_resources <- function(
  aws = NULL,
  clustermq = NULL,
  feather = NULL,
  fst = NULL,
  future = NULL,
  parquet = NULL,
  qs = NULL,
  url = NULL
) {
  envir <- environment()
  names <- names(formals(tar_resources))
  out <- list()
  for (name in names) {
    value <- envir[[name]]
    class <- paste0("tar_resources_", name)
    message <- paste0(
      name,
      " argument to tar_resources() must be output from tar_resources_",
      name,
      "() or NULL."
    )
    if (!is.null(value)) {
      tar_assert_inherits(value, class, message)
      out[[name]] <- value
    }
  }
  out
}
