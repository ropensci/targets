test_that("time_seconds_local()", {
  tar_runtime$nanonext <- NULL
  for (i in seq_len(4)) {
    out <- time_seconds_local()
    expect_true(is.numeric(out))
    expect_false(anyNA(out))
    expect_equal(length(out), 1L)
  }
  expect_equal(2 * 2, 4)
})

test_that("time stamps", {
  skip_cran()
  expect_s3_class(posixct_time(time_stamp()), "POSIXct")
})
