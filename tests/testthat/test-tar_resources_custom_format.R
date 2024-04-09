tar_test("tar_resources_custom_format()", {
  out <- tar_resources_custom_format()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_custom_format() defaults", {
  tar_option_set(
    resources = tar_resources(
      custom_format = tar_resources_custom_format()
    )
  )
  out <- tar_resources_custom_format()
  expect_null(out$envvars)
})

tar_test("tar_resources_custom_format() non-defaults", {
  tar_option_set(
    resources = tar_resources(
      custom_format = tar_resources_custom_format(
        envvars = c(x = "x")
      )
    )
  )
  out <- tar_resources_custom_format()
  expect_equal(out$envvars, c(x = "x"))
})
