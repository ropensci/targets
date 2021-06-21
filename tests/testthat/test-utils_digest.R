tar_test("digest functions", {
  expect_silent(tar_assert_chr(digest_chr64("x")))
  expect_silent(tar_assert_scalar(digest_chr64("x")))
  expect_silent(tar_assert_chr(digest_obj64(0L)))
  expect_silent(tar_assert_scalar(digest_obj64(0L)))
  expect_error(digest_chr64(0L))
})
