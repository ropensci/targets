tar_test("validate workspace class", {
  tar_script({
    tar_option_set(workspaces = "z")
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  workspace <- workspace_read("z", path_store_default())
  expect_silent(workspace_validate(workspace))
})
