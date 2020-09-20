tar_test("tar_ask_env()", {
  Sys.setenv(TAR_ASK = "")
  expect_null(tar_ask_env())
  Sys.setenv(TAR_ASK = "true")
  expect_true(tar_ask_env())
  Sys.setenv(TAR_ASK = "false")
  expect_false(tar_ask_env())
})

tar_test("tar_should_overwrite", {
  tmp <- tempfile()
  expect_true(tar_should_overwrite(ask = TRUE, file = tmp))
  expect_true(tar_should_overwrite(ask = FALSE, file = tmp))
  file.create(tmp)
  expect_true(tar_should_overwrite(ask = FALSE, file = tmp))
  # More testing is in tests/interactive/test-tar_script.R # nolint.
})
