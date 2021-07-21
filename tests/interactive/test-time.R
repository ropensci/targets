tar_test("time of completed stem updates", {
  tar_script(tar_target(x, 123))
  tar_make(callr_function = NULL)
  time1 <- tar_meta(x, time)$time[[1]]
  Sys.sleep(5)
  tar_script(tar_target(x, 456))
  tar_make(callr_function = NULL)
  time2 <- tar_meta(x, time)$time[[1]]
  expect_false(identical(time1, time2))
})

tar_test("time of errored stem stays the same", {
  tar_script(tar_target(x, 123))
  tar_make(callr_function = NULL)
  time1 <- tar_meta(x, time)$time[[1]]
  Sys.sleep(5)
  tar_script(tar_target(x, stop(123)))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  time2 <- tar_meta(x, time)$time[[1]]
  expect_equal(time1, time2)
})

tar_test("time of canceled stem stays the same", {
  tar_script(tar_target(x, 123))
  tar_make(callr_function = NULL)
  time1 <- tar_meta(x, time)$time[[1]]
  Sys.sleep(5)
  tar_script(tar_target(x, tar_cancel()))
  tar_make(callr_function = NULL)
  time2 <- tar_meta(x, time)$time[[1]]
  expect_equal(time1, time2)
})

tar_test("time of completed branch updates", {
  tar_script({
    list(
      tar_target(x, 1),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  time1 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  Sys.sleep(5)
  tar_script({
    list(
      tar_target(x, 2),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  time2 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  expect_false(identical(time1, time2))
})

tar_test("time of errored branch stays the same", {
  tar_script({
    list(
      tar_target(x, 1),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  time1 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  expect_false(is.na(time1))
  Sys.sleep(5)
  tar_script({
    list(
      tar_target(x, stop(123)),
      tar_target(y, x, pattern = map(x))
    )
  })
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  time2 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  expect_equal(time1, time2)
})

tar_test("time of canceled branch stays the same", {
  tar_script({
    list(
      tar_target(x, 1),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  time1 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  expect_false(is.na(time1))
  Sys.sleep(5)
  tar_script({
    list(
      tar_target(x, tar_cancel()),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  time2 <- tar_meta(all_of(tar_branch_names(y, 1)), time)$time[[1]]
  expect_equal(time1, time2)
})
