#' @title Set up GitHub Actions to run a targets pipeline
#' @export
#' @family scripts
#' @description Writes a GitHub Actions workflow file so the pipeline
#'   runs on every push to GitHub. Historical runs accumulate in the
#'   `targets-runs` branch, and the latest output is restored before
#'   [tar_make()] so up-to-date targets do not rerun.
#' @details Steps to set up continuous deployment:
#'   1. Ensure your pipeline stays within the resource limitations of
#'     GitHub Actions and repositories, both for storage and compute.
#'     For storage, you may wish to reduce the burden with
#'     AWS-backed storage formats like `"aws_qs"`.
#'   2. Ensure Actions are enabled in your GitHub repository.
#'     You may have to visit the Settings tab.
#'   2. Call `targets::tar_renv(extras = character(0))`
#'     to expose hidden package dependencies.
#'   3. Set up `renv` for your project (with `renv::init()`
#'     or `renv::snapshot()`). Details at
#'     <https://rstudio.github.io/renv/articles/ci.html>.
#'   4. Commit the `renv.lock` file to the `main` (recommended)
#'     or `master` Git branch.
#'   5. Run `tar_github_actions()` to create the workflow file.
#'     Commit this file to `main` (recommended) or `master` in Git.
#'   6. Push your project to GitHub. Verify that a GitHub Actions
#'     workflow runs and pushes results to `targets-runs`.
#'     Subsequent runs will only recompute the outdated targets.
#' @return Nothing (invisibly). This function writes a GitHub Actions
#'   workflow file as a side effect.
#' @param path Character of length 1, file path to write the GitHub Actions
#'   workflow file.
#' @param ask Logical, whether to ask before writing if the workflow file
#'   already exists. If `NULL`, defaults to `Sys.getenv("TAR_ASK")`.
#'   (Set to `"true"` or `"false"` with `Sys.setenv()`).
#'   If `ask` and the `TAR_ASK` environment variable are both
#'   indeterminate, defaults to `interactive()`.
#' @examples
#' tar_github_actions(tempfile())
tar_github_actions <- function(
  path = file.path(".github", "workflows", "targets.yaml"),
  ask = NULL
) {
  assert_chr(path, "path must be a character")
  assert_scalar(path, "path must have length 1")
  if (!tar_should_overwrite(ask, path)) {
    # covered in tests/interactive/test-tar_github_actions.R # nolint
    return(invisible()) # nocov
  }
  dir_create(dirname(path))
  source <- system.file("targets.yaml", package = "targets", mustWork = TRUE)
  file.copy(source, path, overwrite = TRUE)
  invisible()
}
