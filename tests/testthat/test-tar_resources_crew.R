tar_test("tar_resources_crew()", {
  out <- tar_resources_crew()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_crew() defaults", {
  tar_option_set(
    resources = tar_resources(
      crew = tar_resources_crew()
    )
  )
  out <- tar_resources_crew()
  expect_null(out$controller)
  expect_true(out$scale)
  expect_null(out$seconds_timeout)
})

tar_test("tar_resources_crew() non-defaults", {
  tar_option_set(
    resources = tar_resources(
      crew = tar_resources_crew(
        controller = "controller_name_x",
        scale = FALSE,
        seconds_timeout = 0.037
      )
    )
  )
  out <- tar_resources_crew()
  expect_equal(out$controller, "controller_name_x")
  expect_false(out$scale)
  expect_equal(out$seconds_timeout, 0.037)
})
