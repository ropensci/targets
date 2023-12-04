tar_test("tar_started() empty", {
  expect_warning(tar_started(), class = "tar_condition_deprecate")
  expect_equal(suppressWarnings(tar_started()), character(0))
  expect_equal(suppressWarnings(tar_started(contains("x"))), character(0))
})

tar_test("tar_started() nonempty", {
  skip_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  path <- path_progress(path_store_default())
  lines <- readLines(path)
  lines <- lines[!grepl("completed", lines)]
  writeLines(lines, path)
  expect_equal(sort(suppressWarnings(tar_started())), sort(c("x", "y")))
  expect_equal(suppressWarnings(tar_started(contains("x"))), "x")
})
