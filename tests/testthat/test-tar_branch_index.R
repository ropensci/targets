tar_test("tar_branch_index()", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, seq_len(4L)),
      tar_target(y, 2L * x, pattern = map(x)),
      tar_target(z, y, pattern = map(y))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  names <- c(
    tar_meta(y, children)$children[[1L]][c(2L, 3L)],
    tar_meta(z, children)$children[[1L]][2L]
  )
  expect_equal(unname(tar_branch_index(names)), c(2L, 3L, 2L))
})

tar_test("bad branch names", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, seq_len(4L)),
      tar_target(y, 2L * x, pattern = map(x)),
      tar_target(z, y, pattern = map(y))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  expect_error(tar_branch_index("abc"), class = "tar_condition_validate")
  branches <- tar_meta(y, children)$children[[1L]][c(2L, 3L)]
  path <- path_meta(path_store_default())
  meta <- readLines(path)
  line <- grep("pattern", grep("^y", meta, value = TRUE), value = TRUE)
  meta <- setdiff(meta, line)
  writeLines(meta, path)
  expect_error(tar_branch_index(branches), class = "tar_condition_validate")
})
