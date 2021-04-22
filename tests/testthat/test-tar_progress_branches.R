tar_test("tar_progress_branches() on empty progress", {
  tar_script(list())
  tar_make(callr_function = NULL)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches()
  expect_equal(dim(out), c(0L, 6L))
})

tar_test("tar_progress_branches()", {
  tar_script({
    list(
      tar_target(x, seq_len(1)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, stopifnot(y > 1.5), pattern = map(y))
    )
  }, ask = FALSE)
  expect_error(tar_make(callr_function = NULL))
  out <- tar_progress_branches()
  expect_equal(nrow(out), 2)
  cols <- c("name", "branches", "started", "built", "errored", "canceled")
  expect_equal(colnames(out), cols)
  out <- tar_progress_branches(names = y)
  expect_equal(nrow(out), 1)
  expect_equal(colnames(out), cols)
  expect_equal(out$name, "y")
  expect_equal(out$branches, 1)
  expect_equal(out$started, 0)
  expect_equal(out$built, 1)
  expect_equal(out$canceled, 0)
  expect_equal(out$errored, 0)
  out <- tar_progress_branches(names = z)
  expect_equal(nrow(out), 1)
  expect_equal(colnames(out), cols)
  expect_equal(out$name, "z")
  expect_equal(out$branches, 1)
  expect_equal(out$started, 0)
  expect_equal(out$built, 0)
  expect_equal(out$canceled, 0)
  expect_equal(out$errored, 1)
})

tar_test("tar_progress_branches() with fields", {
  tar_script({
    list(
      tar_target(x, seq_len(1)),
      tar_target(y, x, pattern = map(x))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches(fields = started)
  exp <- tibble::tibble(name = "y", started = 0L)
  expect_equal(out, exp)
})

tar_test("tar_progress_branches_gt() runs without error.", {
  skip_if_not_installed("gt")
  tar_script({
    list(
      tar_target(x, seq_len(1)),
      tar_target(y, x, pattern = map(x))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches_gt()
  expect_true(inherits(out, "gt_tbl"))
})
