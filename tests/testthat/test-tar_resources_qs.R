tar_test("tar_resources_qs()", {
  out <- tar_resources_qs()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_qs() non-default compression level", {
  tar_option_set(
    resources = tar_resources(
      qs = tar_resources_qs(
        compress_level = 4L
      )
    )
  )
  out <- tar_resources_qs()
  expect_equal(out$compress_level, 4L)
})
