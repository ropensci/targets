# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("AWS S3 + HPC", {
  skip_if_no_aws()
  on.exit({
    aws.s3::delete_object(object = "_targets/objects/a", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects/b", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects/c", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets/objects", bucket = bucket_name)
    aws.s3::delete_object(object = "_targets", bucket = bucket_name)
    aws.s3::delete_bucket(bucket = bucket_name)
  })
  bucket_name <- "targets-testing-aws-bucket"
  aws.s3::put_bucket(bucket = "targets-testing-aws-bucket")
  tar_script({
    library(targets)
    library(future)
    future::plan(future::multisession)
    tar_option_set(
      format = "aws_rds",
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = "targets-testing-aws-bucket"
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
  })
  tar_make_future()
  expect_equal(tar_read(c), 2L)
})
