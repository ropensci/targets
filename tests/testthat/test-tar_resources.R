tar_test("empty resources", {
  expect_equal(tar_resources(), list())
})

tar_test("populated resources", {
  out <- tar_resources(
    aws = resources_aws_init(),
    clustermq = resources_clustermq_init(
      template = list(a = 1, n_cores = 123)
    )
  )
  expect_true(is.list(out))
  expect_equal(sort(names(out)), sort(c("aws", "clustermq")))
  expect_true(inherits(out$aws, "tar_resources_aws"))
  expect_true(inherits(out$clustermq, "tar_resources_clustermq"))
})

tar_test("wrong resources", {
  expect_error(
    tar_resources(aws = resources_qs_init()),
    class = "tar_condition_validate"
  )
})
