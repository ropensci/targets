# Use sparingly to minimize AWS costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("AWS S3 + HPC", {
  skip_if_no_aws()
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  code <- substitute({
    library(targets)
    library(future)
    future::plan(future::multisession)
    tar_option_set(
      format = "rds",
      repository = "aws",
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = bucket_name,
          prefix = "_targets"
        )
      ),
      storage = "worker",
      retrieval = "worker"
    )
    list(
      tar_target(a, 1L),
      tar_target(b, a),
      tar_target(c, a + b)
    )
  }, env = list(bucket_name = bucket_name))
  do.call(tar_script, list(code = code))
  tar_make_future(reporter = "silent")
  expect_equal(tar_read(c), 2L)
})
