tar_test("tar_resources_qs()", {
  out <- tar_resources_qs()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_qs() default preset", {
  tar_option_set(
    resources = tar_resources(
      qs = tar_resources_qs(
        preset = "low"
      )
    )
  )
  out <- tar_resources_qs()
  expect_equal(out$preset, "low")
})
