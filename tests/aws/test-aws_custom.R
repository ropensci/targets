# Use sparingly to minimize AWS costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws with custom format", {
  skip_if_no_aws()
  skip_if_not_installed("torch")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    format <- tar_format(
      read = function(path) {
        torch::torch_load(path)
      },
      write = function(object, path) {
        torch::torch_save(obj = object, path = path)
      },
      marshal = function(object) {
        con <- rawConnection(raw(), open = "wr")
        on.exit(close(con))
        torch::torch_save(object, con)
        rawConnectionValue(con)
      },
      unmarshal = function(object) {
        con <- rawConnection(object, open = "r")
        on.exit(close(con))
        torch::torch_load(con)
      }
    )
    tar_target(
      a,
      torch::torch_tensor(c(1, 2)),
      format = format,
      repository = "aws",
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      ),
      storage = "main",
      retrieval = "main"
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.integer(sum(out)), 3L)
  expect_false(file.exists(file.path("_targets", "objects")))
  expect_false(file.exists(file.path("_targets", "objects", "a")))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/a",
    bucket = bucket_name,
    file = tmp
  )
  out <- torch::torch_load(tmp)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.integer(sum(out)), 3L)
})
