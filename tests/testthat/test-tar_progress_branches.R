tar_test("tar_progress_branches() on empty progress", {
  skip_cran()
  tar_script(list())
  tar_make(callr_function = NULL)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches()
  expect_equal(dim(out), c(0L, 7L))
})

tar_test("tar_progress_branches()", {
  skip_cran()
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
  cols <- c(
    "name",
    "branches",
    "skipped",
    "dispatched",
    "completed",
    "errored",
    "canceled"
  )
  expect_equal(colnames(out), cols)
  out <- tar_progress_branches(names = y)
  expect_equal(nrow(out), 1)
  expect_equal(colnames(out), cols)
  expect_equal(out$name, "y")
  expect_equal(out$branches, 1)
  expect_equal(out$dispatched, 0)
  expect_equal(out$completed, 1)
  expect_equal(out$canceled, 0)
  expect_equal(out$errored, 0)
  out <- tar_progress_branches(names = z)
  expect_equal(nrow(out), 1)
  expect_equal(colnames(out), cols)
  expect_equal(out$name, "z")
  expect_equal(out$branches, 1)
  expect_equal(out$dispatched, 0)
  expect_equal(out$completed, 0)
  expect_equal(out$canceled, 0)
  expect_equal(out$errored, 1)
})

tar_test("tar_progress_branches() with fields", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, seq_len(1)),
      tar_target(y, x, pattern = map(x))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches(fields = dispatched)
  exp <- tibble::tibble(name = "y", dispatched = 0L)
  expect_equal(out, exp)
})

tar_test("tar_progress_branches_gt() runs without error.", {
  skip_cran()
  skip_if_not_installed("gt")
  tar_script({
    list(
      tar_target(x, seq_len(1)),
      tar_target(y, x, pattern = map(x))
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  out <- tar_progress_branches_gt(path_store_default())
  expect_true(inherits(out, "gt_tbl"))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    list(
      tar_target(w, letters)
    )
  }, script = "example/script.R")
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(is.data.frame(tar_progress_branches(store = "example/store")))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/w"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
