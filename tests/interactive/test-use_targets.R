tar_test("use_targets()", {
  use_targets(path = "targets.Rmd", open = FALSE)
  expect_true(file.exists("targets.Rmd"))
})
