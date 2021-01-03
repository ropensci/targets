tar_test("tar_traceback()", {
  tar_script({
    tar_option_set(error = "workspace")
    list(tar_target(y, stop("3c47b24bd4a7ad8e5ce70f05eefe7c9c")))
  })
  try(tar_make(callr_function = NULL), silent = TRUE)
  out <- tar_traceback(y)
  expect_true(any(grepl("3c47b24bd4a7ad8e5ce70f05eefe7c9c", out)))
})

tar_test("tar_traceback()", {
  tar_script({
    tar_option_set(workspace = "z")
    list(tar_target(z, 0))
  })
  tar_make(callr_function = NULL)
  out <- tar_traceback(z)
  expect_null(out)
})
