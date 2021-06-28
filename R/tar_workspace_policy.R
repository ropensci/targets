#' @title Workspace policy
#' @export
#' @family debug
#' @description Control when `targets` saves workspace files.
#' @details See [tar_workspace()] for details
#' @return An object of class `"tar_worspace_policy"`. Supply this object
#'   to the `workspaces` argument of [tar_option_set()] to control
#'   when `targets` saves workspaces.
#' @param always Character vector of names of targets to always save
#'   workspaces. Must not include any elements of `never`.
#' @param never Character vector of names of targets to never save
#'   workspaces. Must not include any elements of `always`.
#' @param error Logical, whether to save a workspace for each
#'   target that throws an error (except for the targets in `never`).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   tar_option_set(workspaces = tar_workspace_policy(error = TRUE))
#'   list(
#'     tar_target(x, "value"),
#'     tar_target(y, x)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_workspaces()
#' tar_workspaces(contains("x"))
#' })
#' }
tar_workspace_policy <- function(
  always = character(0),
  never = character(0),
  error = FALSE
) {
  tar_assert_chr(always)
  tar_assert_chr(never)
  tar_assert_not_in(always, never)
  tar_assert_scalar(error)
  tar_assert_lgl(error)
  workspace_policy_init(
    always = always,
    never = never,
    error = error
  )
}
