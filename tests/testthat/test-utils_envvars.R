tar_test("set_envvars()", {
  names <- paste0("targets_test", seq_len(3))
  on.exit(Sys.unsetenv(names))
  for (envvar in names) {
    expect_equal(Sys.getenv(envvar, unset = "unset"), "unset")
  }
  envvars <- tibble::tibble(name = names, value = c("value1", "value2", ""))
  set_envvars(envvars)
  expect_equal(Sys.getenv("targets_test1", unset = "unset"), "value1")
  expect_equal(Sys.getenv("targets_test2", unset = "unset"), "value2")
  expect_equal(Sys.getenv("targets_test3", unset = "unset"), "unset")
})
