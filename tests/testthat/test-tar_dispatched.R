tar_test("tar_dispatched() empty", {
  expect_equal(tar_dispatched(), character(0))
  expect_equal(tar_dispatched(contains("x")), character(0))
})

tar_test("tar_dispatched() nonempty", {
  skip_cran()
  tar_script(list(tar_target(x, 1), tar_target(y, 1)))
  tar_make(callr_function = NULL)
  path <- path_progress(path_store_default())
  lines <- readLines(path)
  lines <- lines[!grepl("completed", lines)]
  writeLines(lines, path)
  expect_equal(sort(tar_dispatched()), sort(c("x", "y")))
  expect_equal(tar_dispatched(contains("x")), "x")
})
