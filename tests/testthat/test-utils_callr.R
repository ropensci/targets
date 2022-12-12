tar_test("callr_args_default", {
  skip_cran()
  expect_message(out <- callr_args_default(callr::r))
  expect_equal(out, tar_callr_args_default(callr::r))
})

tar_test("custom error classes forwarded to top level", {
  skip_cran()
  tar_script(
    tar_target(x, rlang::abort("msg", class = c("my_class_1", "my_class_2")))
  )
  out <- tryCatch(
    tar_make(callr_function = NULL),
    error = function(condition) class(condition)
  )
  expect_true("my_class_1" %in% out)
  expect_true("my_class_2" %in% out)
})

tar_test("custom error classes forwarded to top level", {
  skip_cran()
  tar_script(
    tar_target(x, rlang::abort("msg", class = c("my_class_1", "my_class_2")))
  )
  out <- tryCatch(
    tar_make(callr_arguments = list(show = FALSE), reporter = "silent"),
    error = function(condition) class(condition)
  )
  expect_true("my_class_1" %in% out)
  expect_true("my_class_2" %in% out)
})
