tar_test("Run tar_destroy() interactively.", {
  tar_script()
  tar_make()
  old_tar_ask <- Sys.getenv("TAR_ASK")
  Sys.setenv(TAR_ASK = "true")
  expect_true(file.exists("_targets"))
  tar_destroy() # See a menu and choose 2.
  expect_true(file.exists("_targets"))
  tar_destroy() # See a menu and choose 0.
  expect_true(file.exists("_targets"))
  tar_destroy() # See a menu and choose 1.
  expect_false(file.exists("_targets"))
  Sys.setenv(TAR_ASK = old_tar_ask)
})
