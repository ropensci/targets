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
  expect_true(tar_should_overwrite(path = tmp, ask = TRUE))
  expect_true(tar_should_overwrite(path = tmp, ask = FALSE))
  file.create(tmp)
  expect_true(tar_should_overwrite(path = tmp, ask = FALSE))
  # More testing is in tests/interactive/test-tar_script.R # nolint.
})

tar_test("tar_assert_trust_store() skips if TAR_ASK is false", {
  skip_if(interactive())
  Sys.setenv(TAR_ASK = "false")
  expect_silent(tar_assert_trust_store("_targets"))
})

tar_test("tar_assert_trust_store() skips if the pipeline is active", {
  skip_if(interactive())
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  old <- Sys.getenv("TAR_ACTIVE")
  on.exit(Sys.setenv(TAR_ACTIVE = old))
  Sys.setenv(TAR_ASK = "true", TAR_ACTIVE = "true")
  expect_silent(tar_assert_trust_store("_targets"))
})

tar_test("tar_assert_trust_store() errors if non-interactive", {
  skip_if(interactive())
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  for (value in c("", "true")) {
    Sys.setenv(TAR_ASK = value)
    expect_error(
      tar_assert_trust_store("_targets"),
      class = "tar_condition_run"
    )
  }
})

tar_test("tar_assert_trust_store() skips in R CMD check", {
  skip_if(interactive())
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  on.exit(
    if_any(
      is.na(old),
      Sys.unsetenv("_R_CHECK_PACKAGE_NAME_"),
      Sys.setenv(`_R_CHECK_PACKAGE_NAME_` = old)
    )
  )
  Sys.setenv(TAR_ASK = "true", `_R_CHECK_PACKAGE_NAME_` = "targets")
  expect_true(tar_in_check())
  expect_silent(tar_assert_trust_store("_targets"))
  Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
  expect_false(tar_in_check())
  expect_error(
    tar_assert_trust_store("_targets"),
    class = "tar_condition_run"
  )
})

tar_test("tar_meta_has_custom_code()", {
  meta <- data_frame(
    format = c("rds", NA_character_),
    repository = c("local", NA_character_)
  )
  expect_false(tar_meta_has_custom_code(meta))
  meta$format[2L] <- tar_format(read = function(path) readRDS(path))
  expect_true(tar_meta_has_custom_code(meta))
  meta$format[2L] <- "rds"
  meta$repository[2L] <- "repository_cas&upload=xyz"
  expect_true(tar_meta_has_custom_code(meta))
})

tar_test("mock_outside_check() restores _R_CHECK_PACKAGE_NAME_", {
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA_character_)
  on.exit(
    if_any(
      is.na(old),
      Sys.unsetenv("_R_CHECK_PACKAGE_NAME_"),
      Sys.setenv(`_R_CHECK_PACKAGE_NAME_` = old)
    )
  )
  Sys.setenv(`_R_CHECK_PACKAGE_NAME_` = "targets")
  restore <- mock_outside_check()
  expect_false(tar_in_check())
  restore()
  expect_true(tar_in_check())
})
