tar_test("tar_timestamp() for URLs", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://r-project.org"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://r-project.org", 2),
        format = "url"
      )
    )
  })
  expect_equal(as.numeric(tar_timestamp(abc)), as.numeric(file_time_reference))
  tar_make(callr_function = NULL)
  # correctly parsed posix object
  out <- tar_timestamp(abc)
  expect_equal(length(out), 2L)
  expect_true(inherits(out, "POSIXct"))
  expect_false(anyNA(out))
  # incorrectly parsed posix object
  out <- tar_timestamp(abc, format = "%h %h %h %h %h")
  expect_equal(as.numeric(out), as.numeric(file_time_reference))
  # unparsed time stamp
  out <- tar_timestamp(abc, parse = FALSE, format = "%h %h %h %h %h")
  expect_equal(length(out), 2L)
  expect_false(anyNA(out))
  expect_true(is.character(out))
})
