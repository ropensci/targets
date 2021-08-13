tar_test("tar_started() empty", {
  expect_equal(tar_started(), character(0))
  expect_equal(tar_started(contains("x")), character(0))
})

tar_test("tar_started() nonempty", {
  skip_on_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  path <- path_progress(path_store_default())
  lines <- readLines(path)
  lines <- lines[!grepl("built", lines)]
  writeLines(lines, path)
  expect_equal(sort(tar_started()), sort(c("x", "y")))
  expect_equal(tar_started(contains("x")), "x")
})
