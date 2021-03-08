tar_test("Run tar_script() interactively.", {
  path <- ".github/workflows/targets.yaml"
  expect_false(file.exists(path))
  old_tar_ask <- Sys.getenv("TAR_ASK")
  Sys.setenv(TAR_ASK = "true")
  on.exit(Sys.setenv(TAR_ASK = old_tar_ask))
  tar_github_actions()
  expect_true(file.exists(path))
  tar_github_actions() # See a menu and choose 2.
  expect_true(file.exists(path))
  tar_github_actions() # See a menu and choose 0.
  expect_true(file.exists(path))
  tar_github_actions() # See a menu and choose 1.
  expect_true(file.exists(path))
})
