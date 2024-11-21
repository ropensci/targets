# Use sparingly to minimize AWS costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("aws_qs format data gets stored", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  on.exit(aws_s3_delete_bucket(bucket_name))
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      max_tries = 1L
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 1L
  )
  expect_equal(qs2::qs_read(tmp), "x_value")
})

tar_test("aws_qs format data gets stored with worker storage", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      ),
      storage = "worker",
      retrieval = "worker"
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      max_tries = 1L
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 1L
  )
  expect_equal(qs2::qs_read(tmp), "x_value")
})

tar_test("aws_qs format invalidation", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
  expect_equal(tar_progress(y)$progress, "completed")
  tar_make(callr_function = NULL)
  progress <- tar_progress()
  progress <- progress[progress$progress != "skipped", ]
  expect_equal(nrow(progress), 0L)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      )
    )
    list(
      tar_target(x, "x_value2", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
  expect_equal(tar_progress(y)$progress, "completed")
  expect_equal(tar_read(x), "x_value2")
  expect_equal(tar_read(y), c("x_value2", "y_value"))
})

tar_test("aws_qs format and dynamic branching", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      ),
      storage = "worker",
      retrieval = "worker",
      format = "qs",
      repository = "aws"
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
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      ),
      format = "qs",
      repository = "aws"
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
  skip_if_not_installed("qs2")
  tar_config_set(store = "custom_targets_store")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
    ))
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(file.exists("custom_targets_store"))
  expect_false(file.exists(path_store_default()))
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      max_tries = 1L
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 1L
  )
  expect_equal(qs2::qs_read(tmp), "x_value")
})

tar_test("aws_qs format works with storage = \"none\"", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      )
    )
    list(
      tar_target(
        x,
        qs2::qs_save("x_value", tar_path_target(create_dir = TRUE)),
        format = "qs",
        repository = "aws",
        storage = "none"
      ),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      max_tries = 1L
    )
  )
  expect_false(file.exists(file.path("_targets", "objects", "x")))
  expect_false(file.exists(file.path("_targets", "objects", "y")))
  expect_equal(tar_read(x), "x_value")
  expect_equal(tar_read(y), c("x_value", "y_value"))
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp,
    max_tries = 1L
  )
  expect_equal(qs2::qs_read(tmp), "x_value")
})

tar_test("aws_qs format with custom region", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  region <- region <- ifelse(
    Sys.getenv("AWS_REGION") == "us-east-1",
    "us-east-2",
    "us-east-1"
  )
  cfg <- list(LocationConstraint = region)
  s3$create_bucket(Bucket = bucket_name, CreateBucketConfiguration = cfg)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = ifelse(
            Sys.getenv("AWS_REGION") == "us-east-1",
            "us-east-2",
            "us-east-1"
          ),
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      region = region,
      max_tries = 1L
    )
  )
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/y",
      region = region,
      max_tries = 1L
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
      sprintf("endpoint=%s", base64url::base64_urlencode("NULL")),
      paste0("region=", region),
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
    region = region,
    max_tries = 1L
  )
  expect_equal(qs2::qs_read(tmp), "x_value")
})

tar_test("aws_qs format empty region string", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = "",
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
})

tar_test("aws_qs nonexistent bucket", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          region = "",
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "x_value", format = "qs", repository = "aws"),
      tar_target(y, c(x, "y_value"), format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "x_value")
  aws_s3_delete_bucket(bucket_name)
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
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  on.exit(aws_s3_delete_bucket(bucket_name))
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
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "first", format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), "x")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
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
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "second", format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), "x")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
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
        aws = tar_resources_aws(
          bucket = !!bucket_name,
          prefix = "_targets"
        )
      )
    )
    list(
      tar_target(x, "first", format = "qs", repository = "aws")
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  expect_equal(targets::tar_outdated(callr_function = NULL), "x")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(x)$progress, "completed")
})

tar_test("cloud target paths are not in the file path cache", {
  skip_if_no_aws()
  skip_if_not_installed("qs2")
  s3 <- paws.storage::s3()
  bucket_name <- random_bucket_name()
  on.exit(aws_s3_delete_bucket(bucket_name))
  s3$create_bucket(Bucket = bucket_name)
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        aws = tar_resources_aws(bucket = !!bucket_name, prefix = "_targets")
      )
    )
    list(
      tar_target(w, 1),
      tar_target(x, w, format = "qs", repository = "aws"),
      tar_target(y, sort(names(tar_runtime_object()$file_exist$envir)))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  expect_false(
    path_objects(path_store_default(), "x") %in% tar_read(y)
  )
})
