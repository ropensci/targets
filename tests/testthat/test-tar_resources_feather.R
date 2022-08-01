tar_test("tar_resources_feather()", {
  out <- tar_resources_feather()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_feather() default compression", {
  tar_option_set(
    resources = tar_resources(
      feather = tar_resources_feather(
        compression = "non_default"
      )
    )
  )
  out <- tar_resources_feather()
  expect_equal(out$compression, "non_default")
})

tar_test("tar_resources_feather() default compression_level", {
  tar_option_set(
    resources = tar_resources(
      feather = tar_resources_feather(
        compression_level = 3
      )
    )
  )
  out <- tar_resources_feather()
  expect_equal(out$compression_level, 3)
})
