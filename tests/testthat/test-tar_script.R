tar_test("tar_script() writes example if null", {
  tar_script()
  expect_gt(length(readLines("_targets.R")), 3L)
})

tar_test("tar_script() writes single line exactly", {
  tar_script(single_line, library_targets = FALSE)
  expect_equal(readLines("_targets.R"), "single_line")
})

tar_test("tar_script() writes single line exactly", {
  tar_script({
    multi
    line
  })
  expect_equal(readLines("_targets.R"), c("library(targets)", "multi", "line"))
})
