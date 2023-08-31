# Use sparingly to minimize GCP costs.
# Verify all `targets` buckets are deleted afterwards.
# After this test runs, log into the gcp console,
# check that the prefix is correct, and MANUALLY CLEAR OUT THE BUCKET.
# Run interactively to check the prefix.
tar_test("gcp_parquet format returns data frames", {
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
        gcp = tar_resources_gcp(
          bucket = !!bucket_name,
          prefix = "custom/prefix",
          max_tries = 20
        )
      ),
      format = "parquet",
      repository = "gcp"
    )
    list(
      tar_target(x, data.frame(x = seq_len(2), y = seq_len(2)))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  out <- as.data.frame(tar_read(x))
  expect_equal(out, data.frame(x = seq_len(2), y = seq_len(2)))
  expect_true(
    gcp_gcs_exists(
      key = "custom/prefix/objects/x",
      bucket = bucket_name,
      max_tries = 20L
    )
  )
})
