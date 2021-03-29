tar_test("tar_validate() on a good pipeline", {
  tar_script(list(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("tar_validate() works inside callr", {
  tar_script(list(tar_target(x, 1 + 1)))
  expect_silent(tar_validate(callr_arguments = list(show = FALSE)))
})

tar_test("tar_validate() warns about name conflicts", {
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "true")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    envir <- new.env(parent = globalenv())
    envir$x <- 1
    tar_option_set(envir = envir)
    list(tar_target(x, 1 + 1))
  })
  # The warning happens twice for tar_validate(). # nolint
  suppressWarnings(
    expect_warning(
      tar_validate(callr_function = NULL),
      class = "tar_condition_validate"
    )
  )
})

tar_test("suppress warning about name conflicts", {
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "false")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    envir <- new.env(parent = globalenv())
    envir$x <- 1
    tar_option_set(envir = envir)
    list(tar_target(x, 1 + 1))
  })
  expect_silent(tar_validate(callr_function = NULL))
})
