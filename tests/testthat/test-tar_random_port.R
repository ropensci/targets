tar_test("tar_random_port()", {
  skip_if_not_installed("parallelly")
  expect_true(is.integer(tar_random_port()))
})
