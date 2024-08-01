tar_test("tar_resources_nanoparquet()", {
  out <- tar_resources_nanoparquet()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_nanoparquet() default compression", {
  tar_option_set(
    resources = tar_resources(
      nanoparquet = tar_resources_nanoparquet(
        compression = "zstd"
      )
    )
  )
  out <- tar_resources_nanoparquet()
  expect_equal(out$compression, "zstd")
})

