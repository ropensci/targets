tar_test("tar_resources_parquet()", {
  out <- tar_resources_parquet()
  expect_silent(resources_validate(out))
})
