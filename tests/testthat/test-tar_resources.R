tar_test("empty resources", {
  expect_equal(tar_resources(), list())
})

tar_test("populated resources", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  out <- tar_resources(
    aws = resources_aws_init(),
    gcp = resources_gcp_init(),
    clustermq = resources_clustermq_init(
      template = list(a = 1, n_cores = 123)
    )
  )
  expect_true(is.list(out))
  expect_equal(sort(names(out)), sort(c("aws", "clustermq", "gcp")))
  expect_true(inherits(out$aws, "tar_resources_aws"))
  expect_true(inherits(out$gcp, "tar_resources_gcp"))
  expect_true(inherits(out$clustermq, "tar_resources_clustermq"))
})

tar_test("default resource args", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  tar_option_set(
    resources = tar_resources(
      aws = tar_resources_aws(bucket = "abc"),
      fst = tar_resources_fst(compress = 10L)
    )
  )
  target <- tar_target(
    x,
    1,
    resources = tar_resources(
      aws = tar_resources_aws(bucket = "xyz")
    )
  )
  expect_equal(target$settings$resources$aws$bucket, "xyz")
  expect_equal(target$settings$resources$fst$compress, 10L)
})

tar_test("wrong resources", {
  skip_cran()
  skip_on_os("windows")
  skip_if_not_installed("paws.storage")
  expect_error(
    tar_resources(aws = resources_qs_init()),
    class = "tar_condition_validate"
  )
})
