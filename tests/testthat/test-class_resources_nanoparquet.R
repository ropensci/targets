tar_test("create tar_resources_nanoparquet object", {
  x <- resources_nanoparquet_init(compression = "zstd")
  expect_equal(x$compression, "zstd")
  expect_silent(resources_validate(x))
})

tar_test("prohibit empty tar_resources_nanoparquet object", {
  x <- resources_nanoparquet_init(compression = NULL)
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print tar_resources_nanoparquet object", {
  x <- resources_nanoparquet_init(compression = "zstd")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_nanoparquet", out)))
})
