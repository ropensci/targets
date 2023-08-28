#' @title Delete metadata.
#' @export
#' @family metadata
#' @description Delete the project metadata files from the local file system,
#'   the cloud, or both.
#' @inheritParams tar_meta_sync
#' @param which Character of length 1, which metadata files to delete.
#'   Choose `"local"` for local files, `"cloud"` for files on the cloud,
#'   or `"all"` to delete metadata files from both the local file system
#'   and the cloud.
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
#' tar_meta_delete()
#' })
#' }
tar_meta_delete <- function(
  which = "all",
  verbose = TRUE,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_chr(which)
  tar_assert_scalar(which)
  tar_assert_none_na(which)
  tar_assert_nzchar(which)
  tar_assert_in(which, c("all", "local", "cloud"))
  if (which %in% c("all", "local")) {
    tar_assert_scalar(store)
    tar_assert_chr(store)
    tar_assert_none_na(store)
    tar_assert_nzchar(store)
    unlink(path_meta(store))
    unlink(path_progress(store))
    unlink(path_process(store))
    unlink(path_crew(store))
  }
  if (which %in% c("all", "cloud")) {
    tar_assert_script(script)
    options <- tar_option_script(script = script)
    old_repository_meta <- tar_options$get_repository_meta()
    old_resources <- tar_options$get_resources()
    on.exit({
      tar_options$set_repository_meta(old_repository_meta)
      tar_options$set_resources(old_resources)
    })
    tar_options$set_repository_meta(options$repository_meta)
    tar_options$set_resources(options$resources)
    database_meta(path_store = tempfile())$delete_cloud(verbose = verbose)
    database_progress(path_store = tempfile())$delete_cloud(verbose = verbose)
    database_process(path_store = tempfile())$delete_cloud(verbose = verbose)
    database_crew(path_store = tempfile())$delete_cloud(verbose = verbose)
  }
  invisible()
}
