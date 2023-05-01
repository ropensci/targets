tar_test("trust_object_timestamps = TRUE", {
  skip_if_not_installed("paws")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = TRUE)
  x <- target_init(name = "abc", expr = quote(a), repository = "aws")
  expect_false(x$store$file$trust_timestamps)
})

tar_test("trust_object_timestamps = FALSE", {
  skip_if_not_installed("paws")
  old <- tar_option_get("trust_object_timestamps")
  on.exit(tar_option_set(trust_object_timestamps = old))
  tar_option_set(trust_object_timestamps = FALSE)
  x <- target_init(name = "abc", expr = quote(a), repository = "aws")
  expect_false(x$store$file$trust_timestamps)
})

tar_test("bucket with tech debt: 0.8.1.9000 metabucket format", {
  expect_equal(store_aws_bucket("bucket=abc:region=xyz"), "abc")
})

tar_test("region with tech debt: 0.8.1.9000 metabucket format", {
  expect_equal(store_aws_bucket("bucket=abc:region=xyz"), "abc")
})

tar_test("NULL region", {
  path <- c("bucket=abc", "region=NULL", "key=stuff")
  expect_null(store_aws_region(path))
})

tar_test("\"\" region", {
  path <- c("bucket=abc", "region=", "key=stuff")
  expect_equal(store_aws_region(path), "")
})

tar_test("\"\" endpoint", {
  path <- c("bucket=abc", "endpoint=", "key=stuff")
  expect_equal(store_aws_endpoint(path), "")
})

tar_test("compat with targets <= 0.8.1", {
  path <- c("bucket_name", "object_name", "stage_name")
  expect_equal(store_aws_bucket(path), "bucket_name")
  expect_null(store_aws_region(path))
  expect_equal(store_aws_key(path), "object_name")
})

tar_test("store_aws_key() with targets <= 0.8.1", {
  path <- c("bucket_name", "key_name", "stage_name")
  expect_equal(store_aws_key(path), "key_name")
})

tar_test("store_aws_key() with targets > 0.8.1", {
  path <- c("bucket=b", "region=r", "key=key_name", "stage=stage_name")
  expect_equal(store_aws_key(path), "key_name")
})

tar_test("store_aws_version()", {
  path <- c("bucket=b", "version=number")
  expect_equal(store_aws_version(path), "number")
  expect_null(store_aws_version(letters))
})

tar_test("store_aws_version()", {
  path <- c(
    "bucket=b",
    sprintf("endpoint=%s", base64url::base64_urlencode("answer"))
  )
  expect_equal(store_aws_endpoint(path), "answer")
  expect_null(store_aws_endpoint(letters))
})

tar_test("store_aws_split_colon()", {
  path <- c("bucket=bu:region=reg", "key=sdfasdf")
  expect_equal(
    store_aws_split_colon(path),
    c("bucket=bu", "region=reg", "key=sdfasdf")
  )
  path <- c("key=sdfasdf", "bucket=bu:region=reg")
  expect_equal(
    store_aws_split_colon(path),
    c("bucket=bu", "region=reg", "key=sdfasdf")
  )
})

tar_test("deprecated aws_* classes", {
  skip_cran()
  expect_warning(
    target <- tar_target(x, "x_value", format = "aws_feather"),
    class = "tar_condition_deprecate"
  )
  target2 <- tar_target(x, "x_value", format = "feather", repository = "aws")
  expect_equal(class(target$store), class(target2$store))
})

tar_test("package detection", {
  target <- tar_target(x, "x_value", format = "feather", repository = "aws")
  out <- sort(store_get_packages(target$store))
  exp <- sort(c("paws", "arrow"))
  expect_equal(out, exp)
})

tar_test("inherits from tar_external", {
  store <- tar_target(
    x,
    "x_value",
    format = "feather",
    repository = "aws"
  )$store
  expect_true(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(
    x,
    "x_value",
    format = "feather",
    repository = "aws"
  )$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), "path")
})

tar_test("store_path_from_record()", {
  store <- tar_target(
    x,
    "x_value",
    format = "feather",
    repository = "aws"
  )$store
  record <- record_init(path = "path", format = "aws_feather")
  expect_equal(store_path_from_record(store, record), "path")
})

tar_test("validate aws_feather", {
  skip_cran()
  skip_if_not_installed("paws")
  skip_if_not_installed("arrow")
  target <- tar_target(x, "x_value", format = "feather", repository = "aws")
  tar_script(list(target))
  expect_silent(tar_validate(callr_function = NULL))
})

tar_test("store_produce_path()", {
  store <- tar_target(
    x,
    "x_value",
    format = "rds",
    repository = "aws"
  )$store
  store$resources <- list(bucket = "x_bucket")
  out <- store_produce_path(store, "x_name", "x_object")
  expect_equal(
    sort(out),
    sort(
      c(
        "bucket=x_bucket",
        sprintf("endpoint=%s", base64url::base64_urlencode("NULL")),
        "region=NULL",
        "key=_targets/objects/x_name"
      )
    )
  )
})
