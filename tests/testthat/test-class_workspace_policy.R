tar_test("default workspace policy", {
  x <- workspace_policy_init()
  expect_equal(x$always, character(0))
  expect_equal(x$never, character(0))
  expect_equal(x$error, FALSE)
  expect_silent(workspace_policy_validate(x))
})

tar_test("non-default workspace policy", {
  x <- workspace_policy_init(
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
  x <- workspace_policy_init(
    always = "x",
    never = "x",
    error = TRUE
  )
  expect_equal(x$always, "x")
  expect_equal(x$never, "x")
  expect_equal(x$error, TRUE)
  expect_error(workspace_policy_validate(x), class = "tar_condition_validate")
})

tar_test("invalid workspace policy", {
  x <- workspace_policy_init(
    always = "x",
    never = "y",
    error = "z"
  )
  expect_equal(x$always, "x")
  expect_equal(x$never, "y")
  expect_equal(x$error, "z")
  expect_error(workspace_policy_validate(x), class = "tar_condition_validate")
})
