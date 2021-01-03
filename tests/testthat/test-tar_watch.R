# The real tests of the app are at tests/interactive/test-tar_watch.R # nolint
tar_test("cli_port()", {
  expect_message(cli_port(host = "0.0.0.0", port = "8888"))
})

tar_test("tar_random_port()", {
  expect_true(is.integer(tar_random_port()))
})
