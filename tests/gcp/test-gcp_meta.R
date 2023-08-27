# Use sparingly to minimize gcp costs.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp meta", {
  skip_if_no_gcp()
  skip_if_not_installed("crew")
  bucket_name <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  gcp_gcs_auth(max_tries = 5)
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  # needs to be a GCP project the tester auth has access to
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute({
    library(targets)
    tar_option_set(
      format = "rds",
      repository = "gcp",
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
  }, env = list(bucket_name = bucket_name))
  do.call(tar_script, list(code = code))
  tar_make()
  expect_true(all(tar_progress()$progress == "built"))
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
    gcp_gcs_exists(
      key = file.path("_targets/objects", file),
      bucket = bucket_name,
      max_tries = 5L
    )
  }
  unlink(path_meta(path_store_default()))
  expect_equal(sort(tar_outdated()), sort(c("a", "b", "c")))
  tar_make()
  expect_true(all(tar_progress()$progress == "skipped"))
})
