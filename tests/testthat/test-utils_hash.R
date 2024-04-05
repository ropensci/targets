tar_test("hashing utilities", {
  expect_silent(tar_assert_chr(hash_character("x")))
  expect_silent(tar_assert_scalar(hash_character("x")))
  expect_silent(tar_assert_chr(hash_object(0L)))
  expect_silent(tar_assert_scalar(hash_object(0L)))
  expect_error(hash_character(0L))
})
