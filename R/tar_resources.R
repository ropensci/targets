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
#'
#'   In `targets` version 0.12.2 and above, resources are inherited one-by-one
#'   in nested fashion from `tar_option_get("resources")`.
#'   For example, suppose you set
#'   `tar_option_set(resources = tar_resources(aws = my_aws))`,
#'   where `my_aws` equals `tar_resources_aws(bucket = "x", prefix = "y")`.
#'   Then, `tar_target(data, get_data()` will have bucket `"x"` and
#'   prefix `"y"`. In addition, if `new_resources` equals
#'   `tar_resources(aws = tar_resources_aws(bucket = "z")))`, then
#'   `tar_target(data, get_data(), resources = new_resources)`
#'   will use the new bucket `"z"`, but it will still use the prefix `"y"`
#'   supplied through `tar_option_set()`. (In `targets` 0.12.1 and below,
#'   options like `prefix` do not carry over from `tar_option_set()` if you
#'   supply non-default resources to `tar_target()`.)
#' @return A list of objects of class `"tar_resources"` with
#'   non-default settings of various optional backends for data storage
#'   and high-performance computing.
#' @param aws Output of function `tar_resources_aws()`.
#'   Amazon Web Services (AWS) S3 storage settings for
#'   `tar_target(..., repository = "aws")`.
#'   See the cloud storage section of
#'   <https://books.ropensci.org/targets/data.html>
#'   for details for instructions.
#' @param clustermq Output of function `tar_resources_clustermq()`.
#'   Optional `clustermq` settings for `tar_make_clustermq()`,
#'   including the `log_worker` and `template` arguments of
#'   `clustermq::workers()`. `clustermq` workers are *persistent*,
#'   so there is not a one-to-one correspondence between workers and targets.
#'   The `clustermq` resources apply to the workers, not the targets.
#'   So the correct way to assign `clustermq` resources is through
#'   [tar_option_set()], not [tar_target()]. `clustermq` resources
#'   in individual [tar_target()] calls will be ignored.
#' @param crew Output of function `tar_resources_crew()`
#'   with target-specific settings for integration with the
#'   `crew` R package. These settings are arguments to the `push()`
#'   method of the controller or controller group
#'   object which control things like
#'   auto-scaling behavior and the controller to use in the case
#'   of a controller group.
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
#' @param gcp Output of function `tar_resources_gcp()`.
#'   Google Cloud Storage bucket settings for
#'   `tar_target(..., repository = "gcp")`.
#'   See the cloud storage section of
#'   <https://books.ropensci.org/targets/data.html>
#'   for details for instructions.
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
  aws = tar_option_get("resources")$aws,
  clustermq = tar_option_get("resources")$clustermq,
  crew = tar_option_get("resources")$crew,
  feather = tar_option_get("resources")$feather,
  fst = tar_option_get("resources")$fst,
  future = tar_option_get("resources")$future,
  gcp = tar_option_get("resources")$gcp,
  parquet = tar_option_get("resources")$parquet,
  qs = tar_option_get("resources")$qs,
  url = tar_option_get("resources")$url
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
      resources_validate(value)
    }
  }
  out
}
