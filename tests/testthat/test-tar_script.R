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

tar_test("tar_script() avoids tidy eval by default", {
  command <- "abcdefg"
  tar_script(tar_target(x, !!command))
  expect_true(any(grepl("command", readLines("_targets.R"))))
  expect_false(any(grepl("abcdefg", readLines("_targets.R"))))
})

tar_test("tar_script() can use tidy eval", {
  command <- "abcdefg"
  tar_script(tar_target(x, !!command), tidy_eval = TRUE)
  expect_false(any(grepl("command", readLines("_targets.R"))))
  expect_true(any(grepl("abcdefg", readLines("_targets.R"))))
})
