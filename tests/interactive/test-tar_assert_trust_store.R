tar_test("Trust check menu for custom storage code", {
  tar_script({
    format <- tar_format(
      read = function(path) readRDS(path),
      write = function(object, path) saveRDS(object, path)
    )
    list(tar_target(x, 1L, format = format))
  })
  tar_make(callr_function = NULL)
  old_tar_ask <- Sys.getenv("TAR_ASK")
  on.exit(Sys.setenv(TAR_ASK = old_tar_ask))
  Sys.setenv(TAR_ASK = "true")
  expect_error(tar_read(x)) # See a menu and choose 2.
  expect_error(tar_read(x)) # See a menu and choose 0.
  expect_equal(tar_read(x), 1L) # See a menu and choose 1.
  expect_error(tar_load(x)) # See a menu and choose 2.
  tar_load(x) # See a menu and choose 1.
  expect_equal(x, 1L)
})
