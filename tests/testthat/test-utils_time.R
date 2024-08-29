tar_test("time_seconds_local()", {
  tar_runtime$nanonext <- NULL
  for (i in seq_len(4)) {
    out <- time_seconds_local()
    expect_true(is.numeric(out))
    expect_false(anyNA(out))
    expect_length(out, 1L)
  }
  expect_equal(2 * 2, 4)
})

tar_test("time_stamp()", {
  skip_cran()
  out <- time_stamp(time = Sys.time())
  expect_true(is.character(out))
  expect_false(anyNA(out))
})

tar_test("time_stamp_cli()", {
  skip_cran()
  out <- time_stamp_cli(time = Sys.time())
  expect_true(is.character(out))
  expect_false(anyNA(out))
})

tar_test("time stamp_pid", {
  skip_cran()
  out <- time_stamp_pid(pid = Sys.getpid())
  expect_true(is.character(out))
  expect_length(out, 1L)
  expect_false(anyNA(out))
  out <- time_stamp_pid(pid = -1L)
  expect_true(is.character(out))
  expect_length(out, 1L)
  expect_true(anyNA(out))
})
