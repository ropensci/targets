# Semi-automated tests of Azure Storage
# integration live in tests/azure/.
# These tests should not be fully automated because they
# automatically create buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
azure_blob_exists <- function(container,
  blob_path,
  version = NULL,
  verbose = FALSE
) {
  tryCatch(
    AzureStor::storage_file_exists(
      container = container,
      file = blob_path
    ),
    http_404 = function(condition) {
      FALSE
    }
  )
}

azure_get_token <- function() {
  AzureAuth::get_azure_token(Sys.getenv("RTARGETS_AZURE_APPID"),
    app = Sys.getenv("RTARGETS_AZURE_APPID"),
    tenant = Sys.getenv("RTARGETS_AZURE_TENANT"),
    password = Sys.getenv("RTARGETS_AZURE_PASSWORD")
  )
}

azure_get_storage_account <- function(account_name,
  subscription_id,
  resource_group_name) {

  manager <- AzureRMR::az_rm$new(app = Sys.getenv("RTARGETS_AZURE_APPID"),
    tenant = Sys.getenv("RTARGETS_AZURE_TENANT"),
    password = Sys.getenv("RTARGETS_AZURE_PASSWORD"))

  subscription <- manager$get_subscription(subscription_id)

  resource_group <- subscription$get_resource_group(resource_group_name)

  resource_group$get_storage_account(account_name)
}

azure_create_storage_account <- function(account_name,
  resource_group_name,
  subscription_id) {

  stop("Not yet implemented. Use Azure CLI to create storage account.")

  manager <- AzureRMR::az_rm$new(app = Sys.getenv("RTARGETS_AZURE_APPID"),
    tenant = Sys.getenv("RTARGETS_AZURE_TENANT"),
    password = Sys.getenv("RTARGETS_AZURE_PASSWORD"))

  resource_group <- manager$get_subscription(subscription_id)$
    get_resource_group(resource_group_name)

  resource_group$create_storage_account(account_name,
    kind = "BlobStorage"
    )
}

# nocov end
