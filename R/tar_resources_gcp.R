#' @title Target resources: Google Cloud Platform (GCP)
#'   Google Cloud Storage (GCS)
#' @export
#' @family resources
#' @description Create the `gcp` argument of `tar_resources()`
#'   to specify optional settings for Google Cloud Storage for
#'   targets with `tar_target(..., repository = "gcp")`.
#'   See the `format` argument of [tar_target()] for details.
#' @details See the cloud storage section of
#'   <https://books.ropensci.org/targets/data.html>
#'   for details for instructions.
#' @inheritSection tar_resources Resources
#' @return Object of class `"tar_resources_gcp"`, to be supplied
#'   to the `gcp` argument of `tar_resources()`.
#' @inheritParams tar_resources_aws
#' @param predefined_acl Character of length 1, user access
#'   to the object. See `?googleCloudStorageR::gcs_upload`
#'   for possible values. Defaults to `"private"`.
#' @param max_tries Positive integer of length 1,
#'   number of tries accessing a network resource on GCP.
#' @param verbose Logical of length 1, whether to print
#'   extra messages like progress bars during uploads
#'   and downloads. Defaults to `FALSE`.
#' @examples
#' # Somewhere in you target script file (usually _targets.R):
#' tar_target(
#'   name,
#'   command(),
#'   format = "qs",
#'   repository = "gcp",
#'   resources = tar_resources(
#'     gcp = tar_resources_gcp(
#'       bucket = "yourbucketname",
#'       prefix = "_targets"
#'     ),
#'     qs = tar_resources_qs(preset = "fast"),
#'   )
#' )
tar_resources_gcp <- function(
  bucket = targets::tar_option_get("resources")$gcp$bucket,
  prefix = targets::tar_option_get("resources")$gcp$prefix,
  predefined_acl = targets::tar_option_get("resources")$gcp$predefined_acl,
  max_tries = targets::tar_option_get("resources")$gcp$max_tries,
  verbose = targets::tar_option_get("resources")$gcp$verbose
) {
  if (is.null(prefix)) {
    tar_warn_prefix()
    prefix <- path_store_default()
  }
  predefined_acl <- predefined_acl %|||% "private"
  verbose <- verbose %|||% FALSE
  out <- resources_gcp_init(
    bucket = bucket,
    prefix = prefix,
    predefined_acl = predefined_acl,
    max_tries = max_tries,
    verbose = verbose
  )
  resources_validate(out)
  out
}
