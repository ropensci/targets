tar_test("create tar_resources_network object", {
  x <- resources_network_init()
  expect_silent(resources_validate(x))
})

tar_test("tar_resources_network bad field", {
  x <- resources_network_init(max_tries = character(0))
  expect_error(resources_validate(x), class = "tar_condition_validate")
})

tar_test("print", {
  x <- resources_network_init()
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_network", out)))
})
