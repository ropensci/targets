tar_test("null format is not for users", {
  expect_error(
    tar_target(x, TRUE, format = "null"),
    class = "tar_condition_validate"
  )
})

tar_test("null format works", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, stop("message"), error = "null"),
      tar_target(y, x)
    )
  )
  suppressWarnings(tar_make(callr_function = NULL))
  expect_null(tar_read(x))
  expect_null(tar_read(y))
  expect_false(tar_exist_objects("x"))
  expect_true(tar_exist_objects("y"))
  expect_equal(tar_meta(names = tidyselect::any_of("x"))$format, "null")
  expect_equal(
    sort(tar_outdated(callr_function = NULL)),
    sort(c("x", "y"))
  )
})
