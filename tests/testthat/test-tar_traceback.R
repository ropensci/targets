tar_test("tar_traceback()", {
  remove(list = "traceback", envir = envir_run)
  expect_error(tar_traceback(), class = "condition_validate")
  tar_script(tar_pipeline(tar_target(x, sqrt(sqrt(stop("error message"))))))
  expect_error(tar_make(callr_function = NULL), class = "condition_run")
  out <- tar_traceback()
  expect_true(is.character(out))
  expect_true(any(grepl("error message", out)))
})
