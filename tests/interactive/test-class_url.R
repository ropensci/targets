tar_test("tar_timestamp() for URLs", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  url <- "https://github.com/ropensci/targets"
  skip_if(!url_exists(url))
  tar_script({
    list(
      tar_target(
        abc,
        rep("https://github.com/ropensci/targets", 2),
        format = "url"
      )
    )
  })
  expect_equal(as.numeric(tar_timestamp(abc)), as.numeric(file_time_reference))
  tar_make(callr_function = NULL)
  # correctly parsed posix object
  out <- tar_timestamp(abc)
  expect_equal(length(out), 1L)
  expect_true(inherits(out, "POSIXct"))
  expect_false(anyNA(out))
})
