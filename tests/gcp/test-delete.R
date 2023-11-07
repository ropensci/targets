# Use sparingly to minimize GCP costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("delete cloud targets", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
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
        repository = "gcp"
      ),
      tar_target(
        y,
        x,
        pattern = map(x),
        format = "parquet",
        repository = "gcp"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        gcp_file,
        write_file(tempfile()),
        format = "file",
        repository = "gcp"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "gcp_file")
  name <- tar_meta(name = y, fields = children)$children[[1L]]
  key3 <- path_objects(path_store, name)
  expect_true(gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L))
  expect_true(gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L))
  expect_true(gcp_gcs_exists(key = key3, bucket = bucket_name, max_tries = 5L))
  tar_delete(everything())
  expect_false(
    gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
  )
  expect_false(
    gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L)
  )
  expect_false(
    gcp_gcs_exists(key = key3, bucket = bucket_name, max_tries = 5L)
  )
  expect_true(file.exists("file.txt"))
  expect_message(tar_delete(everything()))
  expect_silent(tar_delete(everything(), verbose = FALSE))
})

tar_test("same with versioning", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(
    bucket_name,
    projectId = project,
    versioning = TRUE
  )
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
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
        repository = "gcp"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        gcp_file,
        write_file(tempfile()),
        format = "file",
        repository = "gcp"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "gcp_file")
  expect_true(
    gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
  )
  expect_true(
    gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L)
  )
  tar_delete(everything())
  expect_false(
    gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
  )
  expect_false(
    gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L)
  )
  expect_true(file.exists("file.txt"))
  expect_silent(tar_delete(everything(), verbose = FALSE))
})

tar_test("tar_destroy() cloud targets", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
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
        repository = "gcp"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        gcp_file,
        write_file(tempfile()),
        format = "file",
        repository = "gcp"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  for (destroy in c("all", "cloud")) {
    tar_make(callr_function = NULL)
    path_store <- path_store_default()
    key1 <- path_objects(path_store, "x")
    key2 <- path_objects(path_store, "gcp_file")
    expect_true(
      gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
    )
    expect_true(
      gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L)
    )
    tar_destroy(destroy = destroy)
    expect_false(
      gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
    )
    expect_false(
      gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L)
    )
    expect_true(file.exists("file.txt"))
  }
  expect_silent(tar_destroy(destroy = "cloud", verbose = FALSE))
})

tar_test("tar_prune(), tar_exist_objects(), and tar_objects() for gcp", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5L)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
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
        repository = "gcp"
      ),
      tar_target(
        local_file,
        write_file("file.txt"),
        format = "file",
        repository = "local"
      ),
      tar_target(
        gcp_file,
        write_file(tempfile()),
        format = "file",
        repository = "gcp"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "gcp_file")
  expect_true(gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L))
  expect_true(gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L))
  expect_equal(
    tar_exist_objects(c("x", "local_file", "gcp_file")),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(tar_objects(), sort(c("x", "gcp_file")))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets")
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
        gcp_file,
        write_file(tempfile()),
        format = "file",
        repository = "gcp"
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_prune(callr_function = NULL)
  path_store <- path_store_default()
  key1 <- path_objects(path_store, "x")
  key2 <- path_objects(path_store, "gcp_file")
  expect_false(
    gcp_gcs_exists(key = key1, bucket = bucket_name, max_tries = 5L)
  )
  expect_true(gcp_gcs_exists(key = key2, bucket = bucket_name, max_tries = 5L))
  expect_equal(
    tar_exist_objects(c("x", "local_file", "gcp_file")),
    c(FALSE, FALSE, TRUE)
  )
  expect_equal(tar_objects(), "gcp_file")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
})
