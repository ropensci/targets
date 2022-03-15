# Use sparingly. We do not want to max out any gcp quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp + HPC", {
  skip_if_no_gcp()
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  code <- substitute({
    library(targets)
    library(future)
    future::plan(future::multisession)
    tar_option_set(
      format = "rds",
      repository = "gcp",
      resources = tar_resources(
        gcp = tar_resources_gcp(
          bucket = bucket_name
        )
      ),
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
  tar_make_future()
  expect_equal(tar_read(c), 2L)
})
