tar_test("tar_resources_network()", {
  out <- tar_resources_network()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_network() non-default resources", {
  tar_option_set(
    resources = tar_resources(
      network = tar_resources_network(
        seconds_interval = 2,
        seconds_timeout = 3,
        max_tries = 5,
        verbose = FALSE
      )
    )
  )
  out <- tar_option_get("resources")$network
  expect_equal(out$seconds_interval, 2)
  expect_equal(out$seconds_timeout, 3)
  expect_equal(out$max_tries, 5)
  expect_false(out$verbose)
})
