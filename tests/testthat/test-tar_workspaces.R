tar_test("tar_workspaces()", {
  expect_equal(tar_workspaces(), character(0))
  tar_script({
    tar_option_set(workspaces = c("x", "y"))
    list(tar_target(x, "value"), tar_target(y, "value"))
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_workspaces(), sort(c("x", "y")))
  expect_equal(tar_workspaces(contains("x")), "x")
})
