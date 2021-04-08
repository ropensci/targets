tar_test("tar_envvars", {
  out <- tar_envvars()
  expect_equal(sort(colnames(out)), sort(c("name", "value")))
  expect_true(all(c("TAR_ASK", "TAR_STORE", "TAR_WARN") %in% out$name))
  Sys.unsetenv("TAR_STORE")
  out <- tar_envvars(unset = NA_character_)
  value <- out$value[out$name == "TAR_STORE"]
  expect_true(is.na(value))
})
