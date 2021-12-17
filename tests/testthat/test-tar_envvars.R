tar_test("tar_envvars", {
  out <- tar_envvars()
  expect_equal(sort(colnames(out)), sort(c("name", "value")))
  expect_true(
    all(
      c("TAR_ASK", "TAR_CONFIG", "TAR_PROJECT", "TAR_WARN") %in% out$name
    )
  )
  value <- Sys.getenv("TAR_ASK")
  on.exit(Sys.setenv(TAR_ASK = value))
  Sys.unsetenv("TAR_ASK")
  out <- tar_envvars(unset = NA_character_)
  value <- out$value[out$name == "TAR_ASK"]
  expect_true(is.na(value))
})
