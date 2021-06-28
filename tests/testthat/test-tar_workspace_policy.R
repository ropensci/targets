tar_test("default workspace policy", {
  x <- tar_workspace_policy()
  expect_equal(x$always, character(0))
  expect_equal(x$never, character(0))
  expect_equal(x$error, FALSE)
  expect_silent(workspace_policy_validate(x))
})

tar_test("non-default workspace policy", {
  x <- tar_workspace_policy(
    always = "x",
    never = "y",
    error = TRUE
  )
  expect_equal(x$always, "x")
  expect_equal(x$never, "y")
  expect_equal(x$error, TRUE)
  expect_silent(workspace_policy_validate(x))
})

tar_test("workspace policy conflicts", {
  expect_error(
    tar_workspace_policy(
      always = "x",
      never = "x",
      error = TRUE
    ),
    class = "tar_condition_validate"
  )
})

tar_test("invalid workspace policy", {
  expect_error(
    tar_workspace_policy(
      always = "x",
      never = "y",
      error = "z"
    ),
    class = "tar_condition_validate"
  )
})
