# Use sparingly to minimize AWS costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("aws file gets stored", {
  skip_if_no_aws()
  bucket_name <- random_bucket_name()
  s3 <- paws::s3()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(memory = "persistent", resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name)
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_aws_file.txt")
        "example_aws_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "aws"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_false(file.exists("example_aws_file.txt"))
  unlink("_targets/scratch", recursive = TRUE)
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_false(file.exists("example_aws_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_true(file.exists("example_aws_file.txt"))
  expect_equal(readLines("example_aws_file.txt"), "x_lines")
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(readLines(tmp), "x_lines")
})

tar_test("aws file gets stored with transient memory", {
  skip_if_no_aws()
  bucket_name <- random_bucket_name()
  s3 <- paws::s3()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(memory = "transient", resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name)
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_aws_file.txt")
        "example_aws_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "aws"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_false(file.exists("example_aws_file.txt"))
  unlink("_targets/scratch", recursive = TRUE)
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_false(file.exists("example_aws_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_true(file.exists("example_aws_file.txt"))
  expect_equal(readLines("example_aws_file.txt"), "x_lines")
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(readLines(tmp), "x_lines")
})

# Run once with debug(store_unload.tar_aws_file) # nolint
# to make sure scratch file gets unloaded according to
# the `memory` setting. Should run more twice for persistent
# and then four times for transient.
tar_test("aws_file format invalidation", {
  skip_if_no_aws()
  s3 <- paws::s3()
  for (memory in c("persistent", "transient")) {
    # print(memory) # Uncomment for debug() test. # nolint
    bucket_name <- random_bucket_name()
    s3$create_bucket(Bucket = bucket_name)
    expr <- quote({
      tar_option_set(
        resources = tar_resources(
          aws = tar_resources_aws(bucket = !!bucket_name)
        ),
        memory = !!memory
      )
      evalq(
        write_local_file <- function(lines) {
          writeLines(lines, "example_aws_file.txt")
          "example_aws_file.txt"
        }, envir = tar_option_get("envir")
      )
      list(
        tar_target(
          x,
          write_local_file("x_lines"),
          format = "file",
          repository = "aws"
        ),
        tar_target(y, readLines(x))
      )
    })
    expr <- tar_tidy_eval(expr, environment(), TRUE)
    eval(as.call(list(`tar_script`, expr, ask = FALSE)))
    tar_make(callr_function = NULL, envir = tar_option_get("envir"))
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
        ),
        memory = !!memory
      )
      evalq(
        write_local_file <- function(lines) {
          writeLines(lines, "example_aws_file.txt")
          "example_aws_file.txt"
        }, envir = tar_option_get("envir")
      )
      list(
        tar_target(
          x,
          write_local_file("x_lines2"),
          format = "file",
          repository = "aws"
        ),
        tar_target(y, readLines(x))
      )
    })
    expr <- tar_tidy_eval(expr, environment(), TRUE)
    eval(as.call(list(`tar_script`, expr, ask = FALSE)))
    tar_make(callr_function = NULL, envir = tar_option_get("envir"))
    expect_equal(tar_progress(x)$progress, "built")
    expect_equal(tar_progress(y)$progress, "built")
    expect_equal(tar_read(y), "x_lines2")
    unlink("example_aws_file.txt")
    aws_s3_delete_bucket(bucket_name)
  }
})

tar_test("aws_file format with a custom data store", {
  skip_if_no_aws()
  tar_config_set(store = "custom_targets_store")
  bucket_name <- random_bucket_name()
  s3 <- paws::s3()
  s3$create_bucket(Bucket = bucket_name)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name)
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_aws_file.txt")
        "example_aws_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "aws"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(file.exists("custom_targets_store"))
  expect_false(file.exists(path_store_default()))
  expect_true(
    aws_s3_exists(bucket = bucket_name, key = "_targets/objects/x")
  )
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("custom_targets_store/scratch/")), 0L)
  expect_false(file.exists("example_aws_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("custom_targets_store/scratch/")), 0L)
  expect_true(file.exists("example_aws_file.txt"))
  expect_equal(readLines("example_aws_file.txt"), "x_lines")
  tmp <- tempfile()
  aws_s3_download(
    key = "_targets/objects/x",
    bucket = bucket_name,
    file = tmp
  )
  expect_equal(readLines(tmp), "x_lines")
})

tar_test("aws_file format file with different region", {
  skip_if_no_aws()
  bucket_name <- random_bucket_name()
  s3 <- paws::s3()
  region <- "us-west-2"
  cfg <- list(LocationConstraint = region)
  s3$create_bucket(Bucket = bucket_name, CreateBucketConfiguration = cfg)
  on.exit(aws_s3_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(resources = tar_resources(
      aws = tar_resources_aws(bucket = !!bucket_name, region = "us-west-2")
    ))
    evalq(
      write_local_file <- function(lines) {
        writeLines(lines, "example_aws_file.txt")
        "example_aws_file.txt"
      }, envir = tar_option_get("envir")
    )
    list(
      tar_target(
        x,
        write_local_file("x_lines"),
        format = "file",
        repository = "aws"
      ),
      tar_target(y, readLines(x))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_true(
    aws_s3_exists(
      bucket = bucket_name,
      key = "_targets/objects/x",
      region = "us-west-2"
    )
  )
  unlink("_targets/scratch", recursive = TRUE)
  expect_equal(tar_read(y), "x_lines")
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_false(file.exists("example_aws_file.txt"))
  path <- tar_read(x)
  expect_equal(length(list.files("_targets/scratch/")), 0L)
  expect_false(file.exists("_targets/scratch/x"))
  expect_true(file.exists("example_aws_file.txt"))
  expect_equal(readLines("example_aws_file.txt"), "x_lines")
  out <- sort(tar_meta(x)$path[[1]])
  exp <- sort(
    c(
      sprintf("bucket=%s", bucket_name),
      sprintf("endpoint=%s", base64url::base64_urlencode("NULL")),
      "region=us-west-2",
      "key=_targets/objects/x",
      "stage=example_aws_file.txt",
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
  expect_equal(readLines(tmp), "x_lines")
  unlink("example_aws_file.txt")
})
