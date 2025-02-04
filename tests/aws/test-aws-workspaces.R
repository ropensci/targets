# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws workspaces are uploaded and downloaded", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          prefix = "_targets",
          max_tries = 20
        )
      ),
      format = "parquet",
      repository = "aws",
      repository_meta = "aws"
    )
    list(
      tar_target(x, stop("this is an error"))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  path <- "_targets/workspaces/x"
  expect_false(aws_s3_exists(key = path, bucket = bucket_name))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  expect_true(aws_s3_exists(key = path, bucket = bucket_name))
  unlink(path)
  expect_false(file.exists(path))
  expect_error(tar_workspace(x), class = "tar_condition_validate")
  tar_workspace_download(x)
  expect_true(file.exists(path))
  expect_silent(tar_workspace(x))
  tar_destroy()
  expect_false(aws_s3_exists(key = path, bucket = bucket_name))
  expect_error(tar_workspace_download(x), class = "http_404")
})
