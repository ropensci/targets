tar_test("units_seconds()", {
  expect_equal(units_seconds(NA), "")
  expect_true(grepl("years", units_seconds(3600 * 24 * 365 + 1)))
  expect_true(grepl("months", units_seconds(3600 * 24 * 30 + 1)))
  expect_true(grepl("months", units_seconds(3600 * 24 * 30 + 1)))
  expect_true(grepl("days", units_seconds(3600 * 24 + 1)))
  expect_true(grepl("hours", units_seconds(3600 + 1)))
  expect_true(grepl("minutes", units_seconds(60 + 1)))
  expect_true(grepl("seconds", units_seconds(1)))
})

tar_test("units_bytes()", {
  expect_equal(units_bytes(NA), "")
  expect_true(grepl("terabytes", units_bytes(1e12 + 1)))
  expect_true(grepl("gigabytes", units_bytes(1e9 + 1)))
  expect_true(grepl("megabytes", units_bytes(1e6 + 1)))
  expect_true(grepl("kilobytes", units_bytes(1e3 + 1)))
  expect_true(grepl("bytes", units_bytes(1)))
})

tar_test("units_branches()", {
  out <- units_branches(c(NA_integer_, 3))
  expect_equal(out, c("", "3 branches"))
})
