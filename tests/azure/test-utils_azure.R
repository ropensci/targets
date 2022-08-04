
tar_test("azure_storage_exists()", {

  skip_if_no_azure()
  container_name <- random_container_name()

  storage_account <- azure_get_storage_account(
    account_name = Sys.getenv("RTARGETS_AZURE_STORAGE_ACCOUNT"),
    subscription_name = Sys.getenv("AZURE_SUBSCRIPTONID"),
    resource_group_name = Sys.getenv("RTARGETS_AZURE_RESOURCEGROUP"))

  # create container
  container <- AzureStor::create_blob_container(
    name = container_name,
    endpoint = storage_account$get_blob_endpoint())

  expect_false(azure_blob_exists(blob_path = "x", container = container))

  # upload file
  tmp_file <- tempfile()
  writeLines("x", tmp_file)
  AzureStor::upload_blob(
    tmp_file,
    "x",
    container = container,
  )

  expect_true(azure_blob_exists(blob_path = "x", container = container))

  # delete container on exit
  on.exit(AzureStor::delete_blob_container(
        name = container_name,
        endpoint = storage_account$get_blob_endpoint(),
        confirm = FALSE))

})