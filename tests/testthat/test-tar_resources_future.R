tar_test("tar_resources_future()", {
  out <- tar_resources_future()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_future() default resources", {
  tar_option_set(
    resources = tar_resources(
      future = tar_resources_future(
        resources = list(a = "x")
      )
    )
  )
  out <- tar_resources_future()
  expect_equal(out$resources$a, "x")
})
