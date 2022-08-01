tar_test("tar_resources_url()", {
  out <- tar_resources_url()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_url() default handle", {
  skip_if_not_installed("curl")
  tar_option_set(
    resources = tar_resources(
      url = tar_resources_url(
        handle = curl::new_handle()
      )
    )
  )
  out <- tar_resources_url()
  expect_true(inherits(out$handle, "curl_handle"))
})
