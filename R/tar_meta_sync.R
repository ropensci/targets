#' @title Synchronize cloud metadata.
#' @export
#' @family metadata
#' @description Synchronize metadata in a cloud bucket with metadata in the
#'   local data store.
#' @details [tar_meta_sync()] synchronizes the local and cloud copies
#'   of all the metadata files of the pipeline so that both have the
#'   most recent copy. For each metadata file,
#'   if the local file does not exist or is older than the cloud file,
#'   then the cloud file is downloaded to the local file path.
#'   Conversely, if the cloud file is older or does not exist, then the local
#'   file is uploaded to the cloud. If the time stamps of these files are
#'   equal, use the `prefer_local` argument to determine
#'   which copy takes precedence.
#' @inheritParams tar_validate
#' @param prefer_local Logical of length 1 to control which copy of each
#'   metadata file takes precedence if the local hash and cloud hash
#'   are different but the time stamps are the same. Set to `TRUE`
#'   to upload the local data file in that scenario, `FALSE` to download
#'   the cloud file.
#' @param verbose Logical of length 1, whether to print informative
#'   console messages.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#>   tar_option_set(
#>     resources = tar_resources(
#>       aws = tar_resources_aws(
#>         bucket = "YOUR_BUCKET_NAME",
#>         prefix = "YOUR_PROJECT_NAME"
#>       )
#>     ),
#>     repository = "aws"
#>   )
#>   list(
#>     tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
#>   )
#' }, ask = FALSE)
#' tar_make()
#' tar_meta_sync()
#' })
#' }
tar_meta_sync <- function(
  prefer_local = TRUE,
  verbose = TRUE,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_lgl(prefer_local)
  tar_assert_none_na(prefer_local)
  tar_assert_scalar(prefer_local)
  tar_assert_lgl(verbose)
  tar_assert_none_na(verbose)
  tar_assert_scalar(verbose)
  tar_assert_script(script)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_none_na(store)
  tar_assert_nzchar(store)
  options <- tar_option_script(script = script)
  old_repository_meta <- tar_options$get_repository_meta()
  old_resources <- tar_options$get_resources()
  on.exit({
    tar_options$set_repository_meta(old_repository_meta)
    tar_options$set_resources(old_resources)
  })
  tar_options$set_repository_meta(options$repository_meta)
  tar_options$set_resources(options$resources)
  meta <- database_meta(path_store = store)
  progress <- database_progress(path_store = store)
  process <- database_process(path_store = store)
  crew <- database_crew(path_store = store)
  meta$sync(prefer_local = prefer_local, verbose = verbose)
  progress$sync(prefer_local = prefer_local, verbose = verbose)
  process$sync(prefer_local = prefer_local, verbose = verbose)
  crew$sync(prefer_local = prefer_local, verbose = verbose)
  invisible()
}
