tar_test("tar_glimpse()", {
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_glimpse(
    callr_function = NULL,
    callr_arguments = list(show = FALSE)
  )
  expect_true(inherits(out, "visNetwork"))
})
