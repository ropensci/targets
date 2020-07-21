tar_test("nchar(cli_progress()) is invariant to count", {
  out1 <- cli_progress(0, 0, 0, 0, 0, 0, 0)
  out2 <- cli_progress(0, 0, 2, 0, 0, 10, 0)
  out3 <- cli_progress(0, 123456789, 0, 123456789, 0, 0, 0)
  expect_equal(nchar(out1), nchar(out2))
  expect_equal(nchar(out2), nchar(out3))
})
