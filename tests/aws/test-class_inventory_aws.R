tar_test("inventory_aws class", {
  bucket <- random_bucket_name()
  client <- paws.storage::s3()
  client$create_bucket(Bucket = bucket)
  on.exit(aws_s3_delete_bucket(bucket))
  prefix <- path_objects_dir(path_store_default())
  head <- list()
  for (key in file.path(prefix, c("w", "x", "y", "z"))) {
    head[[key]] <- client$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket
    )
  }
  inventory <- inventory_aws_init()
  resources <- tar_resources(
    aws = tar_resources_aws(
      bucket = bucket,
      prefix = path_store_default()
    )
  )
  store <- store_init(repository = "aws", resources = resources)
  expect_equal(inventory$list_cache(), character(0L))
  expect_equal(inventory$downloads, 0L)
  expect_equal(inventory$misses, 0L)
  for (key in rev(file.path(prefix, c("w", "x", "y", "z")))) {
    store$file$path <- store_produce_aws_path(
      store = store,
      name = basename(key),
      path_store = path_store_default()
    )
    out <- inventory$get_cache(store)
    expect_equal(inventory$misses, 1L)
    expect_equal(inventory$downloads, 1L)
    expect_equal(out, digest_chr64(head[[key]]$ETag))
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
  store$file$path <- store_produce_aws_path(
    store = store,
    name = "nope",
    path_store = path_store_default()
  )
  expect_null(inventory$get_cache(store))
  expect_equal(inventory$downloads, 1L)
  expect_equal(inventory$misses, 2L)
})
