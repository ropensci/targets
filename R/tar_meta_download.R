#' @title download local metadata to the cloud.
#' @export
#' @family metadata
#' @description download local metadata files to the cloud location
#'   (repository, bucket, and prefix) you set in
#'   [tar_option_set()] in `_targets.R`.
#' @inheritParams tar_meta_sync
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
#' tar_meta_download()
#' })
#' }
tar_meta_download <- function(
  verbose = TRUE,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
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
  database_meta(path_store = store)$download(verbose = verbose)
  database_progress(path_store = store)$download(verbose = verbose)
  database_process(path_store = store)$download(verbose = verbose)
  database_crew(path_store = store)$download(verbose = verbose)
  invisible()
}
