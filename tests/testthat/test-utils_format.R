tar_test("format_seconds()", {
  expect_equal(format_seconds(NA), "")
  expect_true(grepl("years", format_seconds(3600 * 24 * 365 + 1)))
  expect_true(grepl("months", format_seconds(3600 * 24 * 30 + 1)))
  expect_true(grepl("months", format_seconds(3600 * 24 * 30 + 1)))
  expect_true(grepl("days", format_seconds(3600 * 24 + 1)))
  expect_true(grepl("hours", format_seconds(3600 + 1)))
  expect_true(grepl("minutes", format_seconds(60 + 1)))
  expect_true(grepl("seconds", format_seconds(1)))
})

tar_test("format_bytes()", {
  expect_equal(format_bytes(NA), "")
  expect_true(grepl("terabytes", format_bytes(1e12 + 1)))
  expect_true(grepl("gigabytes", format_bytes(1e9 + 1)))
  expect_true(grepl("megabytes", format_bytes(1e6 + 1)))
  expect_true(grepl("kilobytes", format_bytes(1e3 + 1)))
  expect_true(grepl("bytes", format_bytes(1)))
})

tar_test("format_branches()", {
  out <- format_branches(c(NA_integer_, 3))
  expect_equal(out, c("", "3 branches"))
})
