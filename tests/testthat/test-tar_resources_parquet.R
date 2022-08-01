tar_test("tar_resources_parquet()", {
  out <- tar_resources_parquet()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_parquet() default compression", {
  tar_option_set(
    resources = tar_resources(
      parquet = tar_resources_parquet(
        compression = "non_default"
      )
    )
  )
  out <- tar_resources_parquet()
  expect_equal(out$compression, "non_default")
})

tar_test("tar_resources_parquet() default compression_level", {
  tar_option_set(
    resources = tar_resources(
      parquet = tar_resources_parquet(
        compression_level = 3
      )
    )
  )
  out <- tar_resources_parquet()
  expect_equal(out$compression_level, 3)
})
