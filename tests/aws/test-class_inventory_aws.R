tar_test("inventory_aws class with versioning from the buckets", {
  bucket1 <- random_bucket_name()
  bucket2 <- random_bucket_name()
  client <- paws.storage::s3()
  client$create_bucket(Bucket = bucket1)
  client$create_bucket(Bucket = bucket2)
  client$put_bucket_versioning(
    Bucket = bucket2,
    VersioningConfiguration = list(Status = "Enabled")
  )
  on.exit({
    aws_s3_delete_bucket(bucket1)
    aws_s3_delete_bucket(bucket2)
  })
  head1 <- list()
  head2 <- list()
  prefix <- path_objects_dir(path_store_default())
  for (key in file.path(prefix, c("w", "x", "y", "z"))) {
    head1[[key]] <- client$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket1
    )
    head2[[key]] <- client$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket2
    )
  }
  inventory1 <- inventory_aws_init()
  inventory2 <- inventory_aws_init()
  resources <- tar_resources(
    aws = tar_resources_aws(
      bucket = bucket1,
      prefix = path_store_default()
    )
  )
  store <- store_init(repository = "aws", resources = resources)
  store$file$path <- store_produce_aws_path(
    store = store,
    name = "x",
    path_store = path_store_default()
  )
  key <- store_aws_key(store$file$path)
  for (index in seq_len(2L)) {
    out1 <- inventory1$get(
      key = key,
      bucket = bucket1,
      version = head1[[key]]$VersionId %||% NULL,
      store = store
    )
    out2 <- inventory2$get(
      key = key,
      bucket = bucket2,
      version = head2[[key]]$VersionId %||% NULL,
      store = store
    )
    expect_equal(out1, out2)
    expect_false(anyNA(out1))
    expect_true(is.character(out1))
    expect_equal(length(out1), 1L)
    expect_true(nzchar(out1))
    expect_equal(length(as.list(inventory1$cache)), 4L)
    expect_equal(length(as.list(inventory2$cache)), 1L)
  }
})

tar_test("inventory_aws class with versioning from resource settings", {
  bucket1 <- random_bucket_name()
  bucket2 <- random_bucket_name()
  client <- paws.storage::s3()
  client$create_bucket(Bucket = bucket1)
  client$create_bucket(Bucket = bucket2)
  client$put_bucket_versioning(
    Bucket = bucket1,
    VersioningConfiguration = list(Status = "Enabled")
  )
  client$put_bucket_versioning(
    Bucket = bucket2,
    VersioningConfiguration = list(Status = "Enabled")
  )
  on.exit({
    aws_s3_delete_bucket(bucket1)
    aws_s3_delete_bucket(bucket2)
  })
  head1 <- list()
  head2 <- list()
  prefix <- path_objects_dir(path_store_default())
  for (key in file.path(prefix, c("w", "x", "y", "z"))) {
    head1[[key]] <- client$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket1
    )
    head2[[key]] <- client$put_object(
      Body = charToRaw(key),
      Key = key,
      Bucket = bucket2
    )
    expect_true(nzchar(head1[[key]]$VersionId))
    expect_true(nzchar(head2[[key]]$VersionId))
  }
  inventory1 <- inventory_aws_init()
  inventory2 <- inventory_aws_init()
  resources1 <- tar_resources(
    aws = tar_resources_aws(
      bucket = bucket1,
      prefix = path_store_default(),
      version = "latest"
    )
  )
  resources2 <- tar_resources(
    aws = tar_resources_aws(
      bucket = bucket2,
      prefix = path_store_default(),
      version = "meta"
    )
  )
  store1 <- store_init(repository = "aws", resources = resources1)
  store2 <- store_init(repository = "aws", resources = resources2)
  store1$file$path <- store_produce_aws_path(
    store = store1,
    name = "x",
    path_store = path_store_default()
  )
  store2$file$path <- store_produce_aws_path(
    store = store2,
    name = "x",
    path_store = path_store_default()
  )
  key <- store_aws_key(store1$file$path)
  store1$file$path <- c(
    store1$file$path,
    paste0("version=", head1[[key]]$VersionId)
  )
  store2$file$path <- c(
    store2$file$path,
    paste0("version=", head2[[key]]$VersionId)
  )
  for (index in seq_len(2L)) {
    out1 <- inventory1$get(
      key = key,
      bucket = bucket1,
      version = store_aws_version_use(store1, store1$file$path),
      store = store1
    )
    out2 <- inventory2$get(
      key = key,
      bucket = bucket2,
      version = store_aws_version_use(store2, store2$file$path),
      store = store2
    )
    expect_equal(out1, out2)
    expect_false(anyNA(out1))
    expect_true(is.character(out1))
    expect_equal(length(out1), 1L)
    expect_true(nzchar(out1))
    expect_equal(length(as.list(inventory1$cache)), 4L)
    expect_equal(length(as.list(inventory2$cache)), 1L)
  }
})
