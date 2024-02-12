tar_test("use_targets() overwrite", {
  skip_cran()
  script <- tar_config_get("script")
  expect_false(file.exists(script))
  use_targets(overwrite = FALSE, open = FALSE)
  expect_true(file.exists(script))
  lines <- readLines(script)
  writeLines("abc", script)
  use_targets(overwrite = FALSE, open = FALSE)
  expect_equal(readLines(script), "abc")
  use_targets(overwrite = TRUE, open = FALSE)
  expect_equal(readLines(script), lines)
})

tar_test("use_targets() script works", {
  skip_cran()
  script <- tar_config_get("script")
  expect_false(file.exists(script))
  use_targets(overwrite = FALSE, open = FALSE)
  expect_true(file.exists(script))
  tar_make(names = tidyselect::contains("data"), callr_function = NULL)
  expect_true(is.data.frame(tar_read(data)))
})

tar_test("use_targets() deprecations", {
  expect_warning(
    use_targets(scheduler = "sge"),
    class = "tar_condition_deprecate"
  )
  expect_warning(
    use_targets(job_name = "name"),
    class = "tar_condition_deprecate"
  )
})
