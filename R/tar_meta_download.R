#' @title download local metadata to the cloud.
#' @export
#' @family metadata
#' @description download local metadata files to the cloud location
#'   (repository, bucket, and prefix) you set in
#'   [tar_option_set()] in `_targets.R`.
#' @inheritParams tar_meta_sync
#' @param strict Logical of length 1. `TRUE` to error out if the file
#'   does not exist in the bucket, `FALSE` to proceed without an error or
#'   warning. If `strict` is `FALSE` and `verbose` is `TRUE`,
#'   then an informative message will print to the R console.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(
#'     resources = tar_resources(
#'       aws = tar_resources_aws(
#'         bucket = "YOUR_BUCKET_NAME",
#'         prefix = "YOUR_PROJECT_NAME"
#'       )
#'     ),
#'     repository = "aws"
#'   )
#'   list(
#'     tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
#'   )
#' })
#' tar_make()
#' tar_meta_download()
#' })
#' }
tar_meta_download <- function(
  meta = TRUE,
  progress = TRUE,
  process = TRUE,
  crew = TRUE,
  verbose = TRUE,
  strict = FALSE,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_lgl(meta)
  tar_assert_scalar(meta)
  tar_assert_none_na(meta)
  tar_assert_lgl(progress)
  tar_assert_scalar(progress)
  tar_assert_none_na(progress)
  tar_assert_lgl(process)
  tar_assert_scalar(process)
  tar_assert_none_na(process)
  tar_assert_lgl(crew)
  tar_assert_scalar(crew)
  tar_assert_none_na(crew)
  tar_assert_lgl(verbose)
  tar_assert_scalar(verbose)
  tar_assert_none_na(verbose)
  tar_assert_lgl(strict)
  tar_assert_scalar(strict)
  tar_assert_none_na(strict)
  tar_assert_script(script)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_none_na(store)
  tar_assert_nzchar(store)
  options <- tar_script_options(script = script)
  old_repository_meta <- tar_options$get_repository_meta()
  old_resources <- tar_options$get_resources()
  on.exit({
    tar_options$set_repository_meta(old_repository_meta)
    tar_options$set_resources(old_resources)
  })
  tar_options$set_repository_meta(options$repository_meta)
  tar_options$set_resources(options$resources)
  if (meta) {
    database_meta(path_store = store)$nice_download(
      verbose = verbose,
      strict = strict
    )
  }
  if (progress) {
    database_progress(path_store = store)$nice_download(
      verbose = verbose,
      strict = strict
    )
  }
  if (process) {
    database_process(path_store = store)$nice_download(
      verbose = verbose,
      strict = strict
    )
  }
  if (crew) {
    database_crew(path_store = store)$nice_download(
      verbose = verbose,
      strict = strict
    )
  }
  invisible()
}
