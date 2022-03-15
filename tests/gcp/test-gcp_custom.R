# Use sparingly. We do not want to max out any gcp quotas.
# And afterwards, manually verify that all the buckets are gone.
tar_test("gcp with custom format", {
  skip_if_no_gcp()
  skip_if_not_installed("torch")
  bucket_name <- random_bucket_name()
  # needs to be a GCP project the tester auth has access to
  gcp_gcs_auth()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket_name, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket_name))
  expr <- quote({
    format <- tar_format(
      read = function(path) {
        torch::torch_load(path)
      },
      write = function(object, path) {
        torch::torch_save(obj = object, path = path)
      },
      marshal = function(object) {
        con <- rawConnection(raw(), open = "wr")
        on.exit(close(con))
        torch::torch_save(object, con)
        rawConnectionValue(con)
      },
      unmarshal = function(object) {
        con <- rawConnection(object, open = "r")
        on.exit(close(con))
        torch::torch_load(con)
      }
    )
    tar_target(
      a,
      torch::torch_tensor(c(1, 2)),
      format = format,
      repository = "gcp",
      resources = tar_resources(
        gcp = tar_resources_gcp(bucket = !!bucket_name)
      ),
      storage = "main",
      retrieval = "main"
    )
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  eval(as.call(list(`tar_script`, expr, ask = FALSE)))
  tar_make(callr_function = NULL)
  out <- tar_read(a)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.integer(sum(out)), 3L)
  expect_false(file.exists(file.path("_targets", "objects")))
  expect_false(file.exists(file.path("_targets", "objects", "a")))
  tmp <- tempfile()
  gcp_gcs_download(
    key = "_targets/objects/a",
    bucket = bucket_name,
    file = tmp
  )
  out <- torch::torch_load(tmp)
  expect_true(inherits(out, "torch_tensor"))
  expect_equal(as.integer(sum(out)), 3L)
})
