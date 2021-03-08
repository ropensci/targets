tar_test("tar_github_actions()", {
  tmp <- tempfile()
  expect_false(file.exists(tmp))
  tar_github_actions(path = tmp)
  expect_true(file.exists(tmp))
})
