tar_test("digest functions", {
  expect_silent(assert_chr(digest_chr64("x")))
  expect_silent(assert_scalar(digest_chr64("x")))
  expect_silent(assert_chr(digest_obj64(0L)))
  expect_silent(assert_scalar(digest_obj64(0L)))
  expect_error(digest_chr64(0L))
})

tar_test("files_identical()", {
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  writeLines("a", tmp1)
  writeLines("a", tmp2)
  expect_true(files_identical(tmp1, tmp2))
})
