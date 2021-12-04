# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("AWS S3 + HPC", {
  skip_if_no_aws()
  s3 <- paws::s3()
  on.exit({
    s3$delete_object(Key = "_targets/objects/a", Bucket = bucket_name)
    s3$delete_object(Key = "_targets/objects/b", Bucket = bucket_name)
    s3$delete_object(Key = "_targets/objects/c", Bucket = bucket_name)
    s3$delete_object(Key = "_targets/objects", Bucket = bucket_name)
    s3$delete_object(Key = "_targets", Bucket = bucket_name)
    s3$delete_bucket(Bucket = bucket_name)
  })
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  code <- substitute({
    library(targets)
    library(future)
    future::plan(future::multisession)
    tar_option_set(
      format = "aws_rds",
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = bucket_name
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
  tar_make_future()
  expect_equal(tar_read(c), 2L)
})
