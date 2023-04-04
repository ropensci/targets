tar_test("create tar_resources_crew object", {
  x <- resources_crew_init(controller = "x", seconds_timeout = 123)
  expect_silent(resources_validate(x))
})

tar_test("print tar_resources_crew object", {
  x <- resources_crew_init(controller = "x")
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_crew", out)))
})
