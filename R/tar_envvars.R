#' @title Show environment variables to customize `targets`
#' @export
#' @family configuration
#' @description You can customize the behavior of `targets`
#'   with special environment variables. The sections in this help file
#'   describe each environment variable, and the `tar_envvars()` function
#'   lists their current values.
#' @details If you modify environment variables, please set them
#'   in project-level `.Renviron` file so you do not lose your
#'   configuration when you restart your R session.
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
#'   before overwriting certain files, such as the target script file
#'   (default: `_targets.R`) in [tar_script()].
#'   If `TAR_ASK` is `"false"`, then `targets` overwrites the old files
#'   with the new ones without asking. Once you are comfortable with
#'   [tar_script()], [tar_github_actions()], and similar functions,
#'   you can safely set `TAR_ASK` to `"false"` in either a project-level
#'   or user-level `.Renviron` file.
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
  tar_assert_chr(unset, "unset must be a character.")
  tar_assert_scalar(unset, "unset must have length 1.")
  names <- c(
    "TAR_ASK",
    "TAR_WARN"
  )
  values <- map_chr(names, Sys.getenv, unset = unset, names = FALSE)
  tibble::tibble(name = names, value = values)
}
