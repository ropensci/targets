tar_test("tar_resources_url()", {
  out <- tar_resources_url()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_url() default handle", {
  skip_if_not_installed("curl")
  tar_option_set(
    resources = tar_resources(
      url = tar_resources_url(
        handle = curl::new_handle(),
        seconds_interval = 2,
        seconds_timeout = 3
      )
    )
  )
  out <- tar_option_get("resources")$url
  expect_true(inherits(out$handle, "curl_handle"))
  expect_equal(out$seconds_interval, 2)
  expect_equal(out$seconds_timeout, 3)
})
