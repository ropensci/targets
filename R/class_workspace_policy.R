workspace_policy_init <- function(
  always = character(0),
  never = character(0),
  error = FALSE
) {
  workspace_policy_new(
    always = always,
    never = never,
    error = error
  )
}

workspace_policy_new <- function(
  always = NULL,
  never = NULL,
  error = NULL
) {
  force(always)
  force(never)
  force(error)
  enclass(environment(), "tar_workspace_policy")
}

workspace_policy_validate <- function(x) {
  tar_assert_inherits(x, "tar_workspace_policy")
  tar_assert_correct_fields(x, workspace_policy_new)
  tar_assert_chr(x$always)
  tar_assert_chr(x$never)
  tar_assert_not_in(x$always, x$never)
  tar_assert_scalar(x$error)
  tar_assert_lgl(x$error)
}
