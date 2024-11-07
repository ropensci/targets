tar_test("inventory_gcp class", {
  skip_if_no_gcp()
  gcp_gcs_auth(max_tries = 5)
  bucket <- random_bucket_name()
  project <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
  googleCloudStorageR::gcs_create_bucket(bucket, projectId = project)
  on.exit(gcp_gcs_delete_bucket(bucket))
  prefix <- path_objects_dir(path_store_default())
  head <- list()
  for (key in file.path(prefix, c("w", "x", "y", "z"))) {
    file <- tempfile()
    writeLines(key, file)
    head[[key]] <- googleCloudStorageR::gcs_upload(
      file = file,
      name = key,
      bucket = bucket
    )
  }
  inventory <- inventory_gcp_init()
  resources <- tar_resources(
    gcp = tar_resources_gcp(
      bucket = bucket,
      prefix = path_store_default()
    )
  )
  store <- store_init(repository = "gcp", resources = resources)
  file <- file_init()
  expect_equal(inventory$list_cache(), character(0L))
  expect_equal(inventory$downloads, 0L)
  expect_equal(inventory$misses, 0L)
  for (key in rev(file.path(prefix, c("w", "x", "y", "z")))) {
    file$path <- store_produce_gcp_path(
      store = store,
      name = basename(key),
      path_store = path_store_default()
    )
    out <- inventory$get_cache(store, file)
    expect_equal(inventory$misses, 1L)
    expect_equal(inventory$downloads, 1L)
    expect_equal(out, hash_object(head[[key]]$md5))
    expect_equal(
      sort(inventory$list_cache()),
      sort(
        paste0(
          bucket,
          "|",
          file.path(prefix, c("w", "x", "y", "z"))
        )
      )
    )
  }
  file$path <- store_produce_gcp_path(
    store = store,
    name = "nope",
    path_store = path_store_default()
  )
  expect_null(inventory$get_cache(store, file))
  expect_equal(inventory$downloads, 1L)
  expect_equal(inventory$misses, 2L)
})
