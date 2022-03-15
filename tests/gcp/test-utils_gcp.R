tar_test("gcp_gcs_exists()", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  googleCloudStorageR::gcs_upload(tmp, bucket = bucket, name = "x")
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
})

tar_test("gcp_gcs_head()", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  googleCloudStorageR::gcs_upload(
    tmp,
    bucket = bucket,
    name = "x"
  )
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
  head <- gcp_gcs_head(key = "x", bucket = bucket)
  expect_true(inherits(head, "gcs_objectmeta"))
  expect_true(is.character(head$etag))
  expect_true(nzchar(head$etag))
})

tar_test("gcp_gcs_download()", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  googleCloudStorageR::gcs_upload(
    tmp,
    bucket = bucket,
    name = "x"
  )
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  gcp_gcs_download(file = tmp2, key = "x", bucket = bucket)
  expect_equal(readLines(tmp2), "x")
})

tar_test("gcp_gcs_delete()", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  googleCloudStorageR::gcs_upload(tmp, bucket = bucket, name = "x")
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
  gcp_gcs_delete(key = "x", bucket = bucket)
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
})

tar_test("gcp_gcs_delete() with versions", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project,
    versioning = TRUE
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  googleCloudStorageR::gcs_upload(tmp, bucket = bucket, name = "x")
  head <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  v1 <- head$generation
  tmp <- tempfile()
  writeLines("y", tmp)
  googleCloudStorageR::gcs_upload(tmp, bucket = bucket, name = "x")
  head <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  v2 <- head$generation
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v1))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v2))
  gcp_gcs_delete(key = "x", bucket = bucket, version = v1)
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket, version = v1))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v2))
})

tar_test("gcp_gcs_upload() without headers", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket
  )
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
})

tar_test("gcp_gcs_upload() and download with metadata", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket))
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket))
  tmp <- tempfile()
  writeLines("x", tmp)
  gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "custom_metadata")
  )
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
  head <- gcp_gcs_head(key = "x", bucket = bucket)
  expect_equal(head$metadata$custom, "custom_metadata")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  gcp_gcs_download(file = tmp2, key = "x", bucket = bucket)
  expect_equal(readLines(tmp2), "x")
})

tar_test("gcp_gcs upload twice, get the correct version", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project,
    versioning = TRUE
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  tmp <- tempfile()
  writeLines("first", tmp)
  head_first <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "first-meta")
  )
  v1 <- head_first$generation
  writeLines("second", tmp)
  head_second <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "second-meta")
  )
  v2 <- head_second$generation
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v1))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v2))
  v3 <- v1
  while (identical(v3, v1) || identical(v3, v2)) {
    v3 <- paste0(sample.int(n = 9, size = 16, replace = TRUE), collapse = "")
  }
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket, version = v3))
  h1 <- gcp_gcs_head(key = "x", bucket = bucket, version = v1)
  h2 <- gcp_gcs_head(key = "x", bucket = bucket, version = v2)
  expect_equal(h1$generation, v1)
  expect_equal(h2$generation, v2)
  expect_equal(h1$metadata$custom, "first-meta")
  expect_equal(h2$metadata$custom, "second-meta")
  unlink(tmp)
  gcp_gcs_download(file = tmp, key = "x", bucket = bucket, version = v1)
  expect_equal(readLines(tmp), "first")
  gcp_gcs_download(file = tmp, key = "x", bucket = bucket, version = v2)
  expect_equal(readLines(tmp), "second")
})

tar_test("gcp_gcs_upload: upload twice, get the correct version", {
  skip_if_no_gcp()
  auth_gcp()
  bucket <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket,
    projectId = project,
    versioning = TRUE
  )
  on.exit(gcp_gcs_delete_bucket(bucket))
  tmp <- tempfile()
  writeLines("first", tmp)
  head_first <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "first-meta")
  )
  v1 <- head_first$generation
  writeLines("second", tmp)
  head_second <- gcp_gcs_upload(
    file = tmp,
    key = "x",
    bucket = bucket,
    metadata = list("custom" = "second-meta")
  )
  v2 <- head_second$generation
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v1))
  expect_true(gcp_gcs_exists(key = "x", bucket = bucket, version = v2))
  v3 <- v1
  while (identical(v3, v1) || identical(v3, v2)) {
    v3 <- paste0(sample.int(n = 9, size = 16, replace = TRUE), collapse = "")
  }
  expect_false(gcp_gcs_exists(key = "x", bucket = bucket, version = v3))
  h1 <- gcp_gcs_head(key = "x", bucket = bucket, version = v1)
  h2 <- gcp_gcs_head(key = "x", bucket = bucket, version = v2)
  expect_equal(h1$generation, v1)
  expect_equal(h2$generation, v2)
  expect_equal(h1$metadata$custom, "first-meta")
  expect_equal(h2$metadata$custom, "second-meta")
  unlink(tmp)
  gcp_gcs_download(file = tmp, key = "x", bucket = bucket, version = v1)
  expect_equal(readLines(tmp), "first")
  gcp_gcs_download(file = tmp, key = "x", bucket = bucket, version = v2)
  expect_equal(readLines(tmp), "second")
})
