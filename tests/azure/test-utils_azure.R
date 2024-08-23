# Use sparingly to minimize Azure costs.
# Verify all `targets` buckets are deleted afterwards.
tar_test("azure_exists()", {
  bucket <- random_bucket_name()
  # needs to be an Azure endpoint the tester has access to
  subscription <- AzureRMR::get_azure_login("common")$list_subscriptions()[[1]]
  accounts <- subscription$list_storage_accounts()
  for (i in seq(along = accounts)) {
    endpoint <- try(accounts[[i]]$get_blob_endpoint(), silent = TRUE)
    if (inherits(endpoint, "try-error")) {
      next
    } else {
      break
    }
  }
  if (inherits(endpoint, "try-error")) stop()
  AzureStor::create_storage_container(endpoint, name = bucket)
  on.exit(AzureStor::delete_storage_container(endpoint = endpoint,
                                              name = bucket,
                                              confirm = FALSE))
  expect_false(azure_exists(key = "x",
                            bucket = bucket,
                            endpoint = endpoint$url))
  tmp <- tempfile()
  writeLines("x", tmp)
  AzureStor::storage_container(endpoint, name = bucket) |>
    AzureStor::storage_upload(src = tmp, dest = "x")
  expect_true(azure_exists(key = "x", bucket = bucket, endpoint = endpoint$url))
})

tar_test("azure_head()", {
  bucket <- random_bucket_name()
  # needs to be an Azure endpoint the tester has access to
  subscription <- AzureRMR::get_azure_login("common")$list_subscriptions()[[1]]
  accounts <- subscription$list_storage_accounts()
  for (i in seq(along = accounts)) {
    endpoint <- try(accounts[[i]]$get_blob_endpoint(), silent = TRUE)
    if (inherits(endpoint, "try-error")) {
      next
    } else {
      break
    }
  }
  if (inherits(endpoint, "try-error")) stop()
  AzureStor::create_storage_container(endpoint, name = bucket)
  on.exit(AzureStor::delete_storage_container(endpoint = endpoint,
                                              name = bucket,
                                              confirm = FALSE))
  expect_false(azure_exists(key = "x",
                            bucket = bucket,
                            endpoint = endpoint$url))
  tmp <- tempfile()
  writeLines("x", tmp)
  AzureStor::storage_container(endpoint, name = bucket) |>
    AzureStor::storage_upload(src = tmp, dest = "x")
  expect_true(azure_exists(key = "x", bucket = bucket, endpoint = endpoint$url))
  head <- azure_head(key = "x", bucket = bucket, endpoint = endpoint$url)
  expect_true(is.list(head))
  expect_true(is.character(head$etag))
  expect_true(nzchar(head$etag))
})

tar_test("azure_download()", {
  bucket <- random_bucket_name()
  # needs to be an Azure endpoint the tester has access to
  subscription <- AzureRMR::get_azure_login("common")$list_subscriptions()[[1]]
  accounts <- subscription$list_storage_accounts()
  for (i in seq(along = accounts)) {
    endpoint <- try(accounts[[i]]$get_blob_endpoint(), silent = TRUE)
    if (inherits(endpoint, "try-error")) {
      next
    } else {
      break
    }
  }
  if (inherits(endpoint, "try-error")) stop()
  AzureStor::create_storage_container(endpoint, name = bucket)
  on.exit(AzureStor::delete_storage_container(endpoint = endpoint,
                                              name = bucket,
                                              confirm = FALSE))
  expect_false(azure_exists(key = "x",
                            bucket = bucket,
                            endpoint = endpoint$url))
  tmp <- tempfile()
  writeLines("x", tmp)
  AzureStor::storage_container(endpoint, name = bucket) |>
    AzureStor::storage_upload(src = tmp, dest = "x")
  tmp2 <- tempfile()
  expect_false(file.exists(tmp2))
  azure_download(file = tmp2,
                 key = "x",
                 bucket = bucket,
                 endpoint = endpoint$url)
  expect_equal(readLines(tmp2), "x")
})

tar_test("azure_delete()", {
  bucket <- random_bucket_name()
  # needs to be an Azure endpoint the tester has access to
  subscription <- AzureRMR::get_azure_login("common")$list_subscriptions()[[1]]
  accounts <- subscription$list_storage_accounts()
  for (i in seq(along = accounts)) {
    endpoint <- try(accounts[[i]]$get_blob_endpoint(), silent = TRUE)
    if (inherits(endpoint, "try-error")) {
      next
    } else {
      break
    }
  }
  if (inherits(endpoint, "try-error")) stop()
  AzureStor::create_storage_container(endpoint, name = bucket)
  on.exit(AzureStor::delete_storage_container(endpoint = endpoint,
                                              name = bucket,
                                              confirm = FALSE))
  expect_false(azure_exists(key = "x",
                            bucket = bucket,
                            endpoint = endpoint$url))
  tmp <- tempfile()
  writeLines("x", tmp)
  AzureStor::storage_container(endpoint, name = bucket) |>
    AzureStor::storage_upload(src = tmp, dest = "x")
  expect_true(azure_exists(key = "x", bucket = bucket, endpoint = endpoint$url))
  azure_delete(key = "x", bucket = bucket, endpoint = endpoint$url)
  expect_false(azure_exists(key = "x",
                            bucket = bucket,
                            endpoint = endpoint$url))
})

tar_test("azure_list_etags()", {
  bucket <- random_bucket_name()
  # needs to be an Azure endpoint the tester has access to
  subscription <- AzureRMR::get_azure_login("common")$list_subscriptions()[[1]]
  accounts <- subscription$list_storage_accounts()
  for (i in seq(along = accounts)) {
    endpoint <- try(accounts[[i]]$get_blob_endpoint(), silent = TRUE)
    if (inherits(endpoint, "try-error")) {
      next
    } else {
      break
    }
  }
  if (inherits(endpoint, "try-error")) stop()
  AzureStor::create_storage_container(endpoint, name = bucket)
  on.exit(AzureStor::delete_storage_container(endpoint = endpoint,
                                              name = bucket,
                                              confirm = FALSE))
  expect_equal(
    azure_list_etags(prefix = "/", bucket = bucket, endpoint = endpoint$url),
    list()
  )
  tmp <- tempfile()
  writeLines("a", tmp)
  for (key in c("w", "x", "y", "z")) {
    AzureStor::storage_container(endpoint, name = bucket) |>
      AzureStor::storage_upload(src = tmp, dest = key)
  }
  out <- azure_list_etags(prefix = "", bucket = bucket, endpoint = endpoint$url)
  expect_equal(length(out), 4L)
  expect_equal(sort(names(out)), sort(c("w", "x", "y", "z")))
  for (etag in out) {
    expect_true(is.character(etag))
    expect_true(!anyNA(etag))
    expect_equal(length(etag), 1L)
    expect_gt(nchar(etag), 10L)
  }
})
