tar_test("tar_resources_repository_cas()", {
  out <- tar_resources_repository_cas()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_repository_cas() defaults", {
  tar_option_set(
    resources = tar_resources(
      repository_cas = tar_resources_repository_cas()
    )
  )
  out <- tar_resources_repository_cas()
  expect_null(out$envvars)
})

tar_test("tar_resources_repository_cas() non-defaults", {
  tar_option_set(
    resources = tar_resources(
      repository_cas = tar_resources_repository_cas(
        envvars = c(x = "x")
      )
    )
  )
  out <- tar_resources_repository_cas()
  expect_equal(out$envvars, c(x = "x"))
})
