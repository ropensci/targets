#' @title Download a workspace from the cloud.
#' @export
#' @family debug
#' @description Download a workspace file from the cloud
#'   so it can be loaded with [tar_workspace()].
#' @details If `tar_option_get("repository_meta")` is `"aws"` or `"gcp"`, then
#'   [tar_make()] uploads workspaces to the bucket and prefix provided.
#'   Download one of these workspaces with [tar_workspace_download()].
#'   Downloaded workspaces can be loaded the usual way with
#'   [tar_workspace()], and you should see them in
#'   character vector returned by [tar_workspaces()].
#' @return `NULL` (invisibly). Returns an error if the workspace
#'   cannot be downloaded.
#' @inheritParams tar_validate
#' @param name Symbol, name of the target whose workspace to download.
#' @param script Character string, file path to the `_targets.R` file
#'   defining the pipeline. Must be configured with the right `aws`
#'   and `repository_meta` options (in [tar_option_set()])
#'   to support downloading workspaces from the cloud.
#' @param verbose `TRUE` to print an informative message describing the
#'   download, `FALSE` to stay silent.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tmp <- sample(1)
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(
#'     resources = tar_resources(
#'       tar_resources_aws(
#'         bucket = "YOUR_AWS_BUCKET",
#'         prefix = "_targets"
#'       )
#'     ),
#'     repository = "aws",
#'     repository_meta = "aws"
#'   )
#'   list(
#'     tar_target(x, stop("this is an error and thus triggers a workspace"))
#'   )
#' }, ask = FALSE)
#' # The following code throws an error for demonstration purposes.
#' try(tar_make())
#' # Say the workspace file for target x does not exist.
#' unlink("_targets/workspaces/x")
#' file.exists("_targets/workspaces/x")
#' # We can download it with tar_workspace_download()
#' tar_workspace_download(x)
#' file.exists("_targets/workspaces/x")
#' tar_workspace(x)
#' })
#' }
tar_workspace_download <- function(
  name,
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  verbose = TRUE
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_scalar(name)
  tar_assert_nzchar(name)
  tar_assert_scalar(store)
  tar_assert_chr(store)
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
  tar_assert_not_in(
    x = options$repository_meta,
    choices = "local",
    msg = paste(
      "tar_workspace_download() is not supported for",
      "tar_option_get(\"repository_meta\") == \"local\"."
    )
  )
  # Tested in tests/aws/test-aws-workspaces.R.
  # nocov start
  database <- database_meta(path_store = store)
  database$download_workspace(name, store = store, verbose = verbose)
  invisible()
  # nocov end
}
