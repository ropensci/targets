tar_test("progress does not exist", {
  out <- tar_progress_summary()
  for (field in colnames(out)) {
    if (is.numeric(out[[field]])) {
      expect_equal(out[[field]], 0)
    }
  }
  expect_equal(out$since, "no data")
})

tar_test("default progress", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary()
  expect_equal(
    colnames(out),
    c("skipped", "dispatched", "completed", "errored", "canceled", "since")
  )
  expect_equal(out$dispatched, 0L)
  expect_equal(out$completed, 4L)
  expect_equal(out$errored, 2L)
  expect_equal(out$canceled, 5L)
  expect_true(is.character(out$since))
})

tar_test("progress with tidyselect fields", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary(any_of("time"))
  expect_equal(colnames(out), c("time"))
  expect_true(is.character(out$time))
})

tar_test("tar_progress_summary_gt()", {
  skip_cran()
  skip_if_not_installed("gt")
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(x5, seq_len(5)),
      tar_target(y, stop(123), pattern = map(x), error = "continue"),
      tar_target(z, tar_cancel(), pattern = map(x5)),
      tar_target(w, x, pattern = map(x))
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  out <- tar_progress_summary_gt(path_store_default())
  expect_true(inherits(out, "gt_tbl"))
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    {
      list(
        tar_target(w, letters)
      )
    },
    script = "example/script.R"
  )
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(is.data.frame(tar_progress_summary(store = "example/store")))
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
