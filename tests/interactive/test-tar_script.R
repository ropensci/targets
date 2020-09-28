tar_test("Run tar_script() interactively.", {
  expect_false(file.exists("_targets.R"))
  tar_script(x, library_targets = FALSE)
  expect_true(file.exists("_targets.R"))
  expect_equal(readLines("_targets.R"), "x")
  old_tar_ask <- Sys.getenv("TAR_ASK")
  Sys.setenv(TAR_ASK = "true")
  tar_script(y, library_targets = FALSE) # See a menu and choose 2.
  expect_equal(readLines("_targets.R"), "x")
  tar_script(y, library_targets = FALSE) # See a menu and choose 0.
  expect_equal(readLines("_targets.R"), "x")
  tar_script(y, library_targets = FALSE) # See a menu and choose 1.
  expect_equal(readLines("_targets.R"), "y")
  Sys.setenv(TAR_ASK = old_tar_ask)
})
