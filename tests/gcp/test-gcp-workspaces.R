# Use sparingly to minimize gcp costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("gcp workspaces are uploaded and downloaded", {
  skip_if_no_gcp()
  skip_if_not_installed("arrow")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth(max_tries = 5)
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    tar_option_set(
      resources = tar_resources(
        gcp = tar_resources_gcp(
          bucket = !!bucket_name,
          prefix = "_targets",
          max_tries = 20
        )
      ),
      repository = "gcp",
      repository_meta = "gcp"
    )
    list(
      tar_target(x, stop("this is an error"))
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  path <- "_targets/workspaces/x"
  expect_false(gcp_gcs_exists(key = path, bucket = bucket_name))
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  expect_true(gcp_gcs_exists(key = path, bucket = bucket_name))
  unlink(path)
  expect_false(file.exists(path))
  expect_error(tar_workspace(x), class = "tar_condition_validate")
  tar_workspace_download(x)
  expect_true(file.exists(path))
  expect_silent(tar_workspace(x))
  tar_destroy()
  expect_false(gcp_gcs_exists(key = path, bucket = bucket_name))
  expect_error(tar_workspace_download(x), class = "http_404")
})
