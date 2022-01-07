# Use sparingly. We do not want to max out any AWS quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws_qs format data gets stored", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  on.exit(destroy_bucket(bucket_name))
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/y")
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws_qs format data gets stored with worker storage", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      ),
      storage = "worker",
      retrieval = "worker"
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/y")
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws_qs format invalidation", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "x_value2", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(tar_progress(y)$progress, "built")
  expect_equal(tar_read(x), "x_value2")
  expect_equal(tar_read(y), c("x_value2", "y_value"))
})

tar_test("aws_qs format and dynamic branching", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      ),
      storage = "worker",
      retrieval = "worker",
      format = "aws_qs"
    )
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 10L * x, pattern = map(x)),
      tar_target(z, sum(y))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(unname(tar_read(x)), seq_len(2))
  expect_equal(unname(tar_read(y, branches = 1)), 10L)
  expect_equal(unname(tar_read(y, branches = 2)), 20L)
  expect_equal(unname(tar_read(z)), 30L)
})

tar_test("aws timestamp", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      ),
      format = "aws_qs"
    )
    list(
      tar_target(x, seq_len(1))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  out <- tar_timestamp(x)
  expect_true(inherits(out, "POSIXct"))
  expect_identical(as.numeric(out), as.numeric(file_time_reference))
  tar_make(callr_function = NULL)
  out <- tar_timestamp(x)
  expect_true(inherits(out, "POSIXct"))
  expect_false(any(as.numeric(out) == as.numeric(file_time_reference)))
})

tar_test("aws_qs format with an alternative data store", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  tar_config_set(store = "custom_targets_store")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name)
    ))
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(file.exists("custom_targets_store"))
  expect_false(file.exists(path_store_default()))
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/y")
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws_qs format works with storage = \"none\"", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(
        x,
        qs::qsave("x_value", tar_path(create_dir = TRUE)),
        format = "aws_qs",
        storage = "none"
      ),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/y")
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws_qs format with custom region", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  region <- "us-west-2"
  cfg <- list(LocationConstraint = region)
  s3$create_bucket(Bucket = bucket_name, CreateBucketConfiguration = cfg)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, region = !!region)
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      region = "us-west-2"
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      region = "us-west-2"
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  out <- sort(tar_meta(x)$path[[1]])
  exp <- sort(
    c(
      sprintf("bucket=%s", bucket_name),
      "region=us-west-2",
      "key=_targets/objects/x",
      "version="
    )
  )
  expect_equal(out, exp)
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    region = region
  )
  expect_equal(qs::qread(tmp), "x_value")
})

tar_test("aws_qs format empty region string", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(destroy_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, region = "")
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
})

tar_test("aws_qs nonexistent bucket", {
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, region = "")
      )
    )
    list(
      tar_target(x, "x_value", format = "aws_qs"),
      tar_target(y, c(x, "y_value"), format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
  destroy_bucket(bucket_name)
  expect_error(
    tar_make(callr_function = NULL, reporter = "silent"),
    class = "tar_condition_run"
  )
  out <- tar_meta(x, error)$error
  expect_true(nzchar(out))
  expect_false(anyNA(out))
})

tar_test("aws_qs format versioning", {
  # setup
  skip_if_no_aws()
  skip_if_not_installed("qs")
  s3 <- paws::s3()
  bucket_name <- random_bucket_name()
  on.exit(destroy_bucket(bucket_name))
  s3$create_bucket(Bucket = bucket_name)
  s3$put_bucket_versioning(
    Bucket = bucket_name,
    VersioningConfiguration = list(
      MFADelete = "Disabled",
      Status = "Enabled"
    )
  )
  # first version of the pipeline
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "first", format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), "x")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  first_meta <- tempfile()
  file.copy(path_meta(path_store_default()), first_meta)
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "skipped")
  version1 <- store_aws_version(tar_meta(x)$path[[1]])
  # second version of the pipeline
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "second", format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), "x")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "built")
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "skipped")
  version2 <- store_aws_version(tar_meta(x)$path[[1]])
  expect_equal(tar_read(x), "second")
  # revert the metadata
  file.copy(first_meta, path_meta(path_store_default()), overwrite = TRUE)
  expect_equal(tar_read(x), "first")
  # revert the target script file
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name)
      )
    )
    list(
      tar_target(x, "first", format = "aws_qs")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "skipped")
})
