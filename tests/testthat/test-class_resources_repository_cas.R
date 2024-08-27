tar_test("create tar_resources_repository_cas object", {
  x <- resources_repository_cas_init(envvars = c(x = "x"))
  expect_silent(resources_validate(x))
})

tar_test("print tar_resources_repository_cas object", {
  x <- resources_repository_cas_init(envvars = c(x = "x"))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("tar_resources_repository_cas", out)))
})
