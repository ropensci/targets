#' @title Show environment variables to customize `targets`
#' @export
#' @family options
#' @description You can customize the behavior of `targets`
#'   with special environment variables. The sections in this help file
#'   describe each environment variable, and the `tar_envvars()` function
#'   lists their current values.
#' @details If you modify environment variables, please set them
#'   in project-level `.Renviron` file. That way, the same values
#'   are shared among the main session and
#'   the external `callr` processes of [tar_make()] etc.
#'   Modify the project-level `.Renviron` file with
#'   `usethis::edit_r_environ(scope = "project")`. Restart
#'   your R session after you are done editing.
#' @return A data frame with one row per environment variable
#'   and columns with the name and current value of each.
#'   An unset environment variable will have a value of `""`
#'   by default. (Customize with the `unset` argument).
#' @param unset Character of length 1, value to return
#'   for any environment variable that is not set.
#' @examples
#' tar_envvars()
#' @section TAR_ASK:
#'   The `TAR_ASK` environment variable accepts values `"true"` and `"false"`.
#'   If `TAR_ASK` is not set, or if it is set to `"true"`,
#'   then `targets` asks permission in a menu
#'   before overwriting certain files, such as `_targets.R` in [tar_script()].
#'   If `TAR_ASK` is `"false"`, then `targets` overwrites the old files
#'   with the new ones without asking. Once you are comfortable with
#'   [tar_script()], [tar_github_actions()], and similar functions,
#'   you can safely set `TAR_ASK` to `"false"` in either a project-level
#'   or user-level `.Renviron` file.
#' @section TAR_STORE:
#'   `TAR_STORE` controls the location of the local data store of your
#'   pipeline. If you do not set `TAR_STORE`, then the data store lives
#'   in a special `_targets/` folder at the project root.
#'   (The project root is the value of `getwd()` when you run the pipeline
#'   with [tar_make()] or similar.) If you set `TAR_STORE` to a directory path,
#'   then the pipeline writes its files to the path you specify
#'   instead of the default `_targets/` folder. If you set `TAR_STORE`
#'   to a relative path, the path must be relative to the project root.
#'   `TAR_STORE` applies to metadata and ordinary storage formats such as
#'   `"rds"`, but not to external storage formats
#'   such as `"file"` or `"aws_parquet"`.
#'
#'   Do not set `TAR_STORE` unless your use case absolutely requires it
#'   and you fully understand the consequences.
#'   There are two significant risks:
#'
#'   1. Some functions like [tar_read()] run in your local R session,
#'     while others like [tar_make()] run in a fresh new external
#'     `callr` R process in order to ensure reproducibility.
#'     The value of `Sys.getenv("TAR_STORE")` must agree
#'     between these two R sessions. Otherwise, you risk
#'     reading from or writing to the incorrect location.
#'     For best results, set the value of `TAR_STORE` in
#'     a project-level `.Renviron file` (e.g. with
#'     `usethis::edit_r_environ(scope = "project")`)
#'     and restart your main R session.
#'   2. Alternative data store locations generally undermine
#'     reproducibility. With a non-standard storage location,
#'     your project becomes harder for others to read and understand,
#'     and you will have more trouble refreshing your memory
#'     if you have to put it aside for several months and then
#'     return to it cold.
#'
#'   The path at `TAR_STORE` need not exist in advance,
#'   and it does not need to end with `"_targets"`.
#'   However, it must be locally accessible, and it must be writeable.
#'   For performance, it should live in a physical storage
#'   volume with fast read/write access. (Remote network drives on Windows
#'   will cause intractable lag.) RStudio Connect users can set `TAR_STORE`
#'   to a subdirectory of `/mnt/shared-data/` so the data store persists
#'   across multiple deployments of the same application.
#' @section TAR_WARN:
#'   The `TAR_WARN` environment variable accepts values `"true"` and `"false"`.
#'   If `TAR_WARN` is not set, or if it is set to `"true"`,
#'   then `targets` throws warnings in certain edge cases,
#'   such as target/global name conflicts and dangerous use of
#'   `devtools::load_all()`. If `TAR_WARN` is `"false"`, then `targets`
#'   does not throw warnings in these cases.
#'   These warnings are harmless, and they can detect potentially serious
#'   issues with your pipeline, so please do not set `TAR_WARN`
#'   unless your use case absolutely requires it.
tar_envvars <- function(unset = "") {
  assert_chr(unset, "unset must be a character.")
  assert_scalar(unset, "unset must have length 1.")
  names <- c(
    "TAR_ASK",
    "TAR_STORE",
    "TAR_WARN"
  )
  values <- map_chr(names, Sys.getenv, unset = unset, names = FALSE)
  tibble::tibble(name = names, value = values)
}
