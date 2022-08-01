tar_test("tar_resources_clustermq()", {
  out <- tar_resources_clustermq()
  expect_silent(resources_validate(out))
})

tar_test("tar_resources_clustermq() defaults", {
  tar_option_set(
    resources = tar_resources(
      clustermq = tar_resources_clustermq(
        template = list(cores = 2L)
      )
    )
  )
  out <- tar_resources_clustermq()
  expect_equal(out$template$cores, 2L)
})
