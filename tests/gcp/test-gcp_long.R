# Use sparingly to minimize GCP costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("pipeline continuously uploads metadata", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  gcp_gcs_auth(max_tries = 5)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name, prefix = "_targets"),
        network = tar_resources_network(max_tries = 10L)
      ),
      repository = "gcp",
      repository_meta = "gcp"
    )
    list(
      tar_target(a, 1),
      tar_target(
        b,
        {
          Sys.sleep(2)
          a
        }
      ),
      tar_target(
        c,
        {
          Sys.sleep(2)
          b
        }
      ),
      tar_target(
        d,
        {
          Sys.sleep(200)
          c
        }
      )
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  R.utils::withTimeout(
    expr = tar_make(seconds_meta_upload = 1, reporter = "silent"),
    timeout = 30,
    onTimeout = "silent"
  )
  tar_destroy(destroy = "local")
  temp <- tempfile()
  meta <- path_meta(temp)
  gcp_gcs_download(
    file = meta,
    bucket = bucket_name,
    key = "_targets/meta/meta",
    max_tries = 3
  )
  out <- tar_meta(store = temp, targets_only = TRUE)
  expect_equal(sort(out$name), sort(c("a", "b", "c")))
})
