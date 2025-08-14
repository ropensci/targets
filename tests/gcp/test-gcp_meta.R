# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp meta", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  for (object in c("a", "b", "c")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/objects", object),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  unlink(path_meta(path_store_default()))
  expect_equal(sort(tar_outdated()), sort(c("a", "b", "c")))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "skipped"))
  tar_destroy()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  for (object in c("a", "b", "c")) {
    expect_false(
      gcp_gcs_exists(
        key = file.path("_targets/objects", object),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
})

# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp tar_meta_delete()", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  for (object in c("a", "b", "c")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/objects", object),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  tar_meta_delete()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  for (object in c("a", "b", "c")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/objects", object),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
})

# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp tar_meta_upload()", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  tar_meta_delete(delete = "cloud")
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  tar_meta_upload()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
})

# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp tar_meta_download()", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
  tar_meta_delete(delete = "local")
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
  tar_meta_download()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
})

# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp tar_meta_sync() upload", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  tar_meta_delete(delete = "cloud")
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
  tar_meta_sync()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      gcp_gcs_exists(
        key = file.path("_targets/meta", file),
        bucket = bucket_name,
        max_tries = 5L
      )
    )
  }
})

# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp tar_meta_sync() download", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        ),
        controller = crew::crew_controller_local(),
        storage = "worker",
        retrieval = "worker"
      )
      list(
        tar_target(a, 1L),
        tar_target(b, a),
        tar_target(c, a + b)
      )
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  tar_make(reporter = "silent")
  expect_true(all(tar_progress()$progress == "completed"))
  expect_equal(tar_read(c), 2L)
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
  tar_meta_delete(delete = "local")
  for (file in c("meta", "process", "progress", "crew")) {
    expect_false(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
  tar_meta_sync()
  for (file in c("meta", "process", "progress", "crew")) {
    expect_true(
      file.exists(file.path(path_meta_dir(path_store_default()), file))
    )
  }
})

tar_test("GCP tar_meta_sync() download graceful failure", {
  skip_if_no_gcp()
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        )
      )
      list(tar_target(a, 1L))
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  dir.create(path_store_default())
  expect_message(
    tar_meta_download(verbose = TRUE, strict = FALSE),
    class = "tar_condition_run"
  )
  expect_silent(
    tar_meta_download(verbose = FALSE, strict = FALSE)
  )
  for (verbose in c(TRUE, FALSE)) {
    expect_error(
      tar_meta_download(verbose = verbose, strict = TRUE),
      class = "tar_condition_run"
    )
  }
})

tar_test("GCP tar_meta_sync() upload graceful failure", {
  skip_if_no_gcp()
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute(
    {
      library(targets)
      tar_option_set(
        format = "rds",
        repository = "gcp",
        repository_meta = "gcp",
        resources = tar_resources(
          gcp = tar_resources_gcp(
            bucket = bucket_name,
            prefix = "_targets"
          )
        )
      )
      list(tar_target(a, 1L))
    },
    env = list(bucket_name = bucket_name)
  )
  do.call(tar_script, list(code = code))
  dir.create(path_store_default())
  expect_message(
    tar_meta_upload(verbose = TRUE, strict = FALSE),
    class = "tar_condition_run"
  )
  expect_silent(
    tar_meta_upload(verbose = FALSE, strict = FALSE)
  )
  for (verbose in c(TRUE, FALSE)) {
    expect_error(
      tar_meta_upload(verbose = verbose, strict = TRUE),
      class = "tar_condition_run"
    )
  }
})
