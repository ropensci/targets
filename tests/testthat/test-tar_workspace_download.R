tar_test("tar_workspace_download() on a local pipeline", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  expect_error(
    tar_workspace_download(x),
    class = "tar_condition_validate"
  )
})
