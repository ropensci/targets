# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("delete cloud targets", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    write_file <- function(path) {
      writeLines("value", path)
      path
    }
    list(
      tar_target(
        x,
        data.frame(x = 2),
        format = "parquet",
        repository = "aws"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        aws_file,
        write_file(tempfile()),
        format = "file",
        repository = "aws"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "aws_file")
  expect_true(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_true(aws_s3_exists(key = key2, bucket = bucket_name))
  tar_delete(everything())
  expect_false(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_false(aws_s3_exists(key = key2, bucket = bucket_name))
  expect_true(file.exists("file.txt"))
  expect_silent(tar_delete(everything()))
})

tar_test("same with versioning", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  s3$put_bucket_versioning(
    Bucket = bucket_name,
    VersioningConfiguration = list(
      MFADelete = "Disabled",
      Status = "Enabled"
    )
  )
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    write_file <- function(path) {
      writeLines("value", path)
      path
    }
    list(
      tar_target(
        x,
        data.frame(x = 2),
        format = "parquet",
        repository = "aws"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        aws_file,
        write_file(tempfile()),
        format = "file",
        repository = "aws"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "aws_file")
  expect_true(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_true(aws_s3_exists(key = key2, bucket = bucket_name))
  tar_delete(everything())
  expect_false(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_false(aws_s3_exists(key = key2, bucket = bucket_name))
  expect_true(file.exists("file.txt"))
  expect_silent(tar_delete(everything()))
})

tar_test("tar_destroy() cloud targets", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    write_file <- function(path) {
      writeLines("value", path)
      path
    }
    list(
      tar_target(
        x,
        data.frame(x = 2),
        format = "parquet",
        repository = "aws"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        aws_file,
        write_file(tempfile()),
        format = "file",
        repository = "aws"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  for (destroy in c("all", "cloud")) {
    tar_make(callr_function = NULL)
    path_store <- path_store_default()
    key1 <- path_objects(path_store, "x")
    key2 <- path_objects(path_store, "aws_file")
    expect_true(aws_s3_exists(key = key1, bucket = bucket_name))
    expect_true(aws_s3_exists(key = key2, bucket = bucket_name))
    tar_destroy(destroy = destroy)
    expect_false(aws_s3_exists(key = key1, bucket = bucket_name))
    expect_false(aws_s3_exists(key = key2, bucket = bucket_name))
    expect_true(file.exists("file.txt"))
  }
  expect_silent(tar_destroy(destroy = "cloud"))
})

tar_test("tar_prune(), tar_exist_objects(), and tar_objects() for aws", {
  skip_if_no_aws()
  skip_if_not_installed("arrow")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    write_file <- function(path) {
      writeLines("value", path)
      path
    }
    list(
      tar_target(
        x,
        data.frame(x = 2),
        format = "parquet",
        repository = "aws"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        aws_file,
        write_file(tempfile()),
        format = "file",
        repository = "aws"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "aws_file")
  expect_true(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_true(aws_s3_exists(key = key2, bucket = bucket_name))
  expect_equal(
    tar_exist_objects(c("x", "local_file", "aws_file")),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(tar_objects(), sort(c("x", "aws_file")))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    write_file <- function(path) {
      writeLines("value", path)
      path
    }
    list(
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        aws_file,
        write_file(tempfile()),
        format = "file",
        repository = "aws"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_prune(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "aws_file")
  expect_false(aws_s3_exists(key = key1, bucket = bucket_name))
  expect_true(aws_s3_exists(key = key2, bucket = bucket_name))
  expect_equal(
    tar_exist_objects(c("x", "local_file", "aws_file")),
    c(FALSE, FALSE, TRUE)
  )
  expect_equal(tar_objects(), "aws_file")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
