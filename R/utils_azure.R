# Semi-automated tests of Azure Storage
# integration live in tests/azure/.
# These tests should not be fully automated because they
# automatically create buckets and upload data,
# which could put an unexpected and unfair burden on
# external contributors from the open source community.
# nocov start
azure_storage_exists <- function(
) {
  stop('Not yet implemented')
}

azure_storage_head_true <- function(
) {
  stop('Not yet implemented')
}

azure_storage_head <- function(
) {
  stop('Not yet implemented')
}

azure_storage_bucket <- function() {
  stop('Not yet implemented')

}

azure_storage_download <- function() {
  stop('Not yet implemented')
  azure_storage_auth(verbose = verbose)
  AzureStor::download_blob(
    src,
    dest,
    container = container,
  )
}

azure_storage_delete <- function() {
  stop('Not yet implemented')
}

azure_storage_upload <- function(
  file,
  key,
  container
  ) {
  stop('Not yet implemented')
  azure_storage_auth()
  AzureStor::upload_blob(
    src,
    dest,
    container = container,
  )
}

#' Following workflow from: https://github.com/Azure/AzureStor#admin-interface
#' 
azure_storage_auth <- function(subscription_id,
resource_group_id,
storage_account_id) {
  
  az <- AzureRMR::create_azure_login()
  
  subscription <- az$get_subscription(subscription_id)
  
  resource_group <- subscription$get_resource_group(resource_group)

  storage_account <- resource_group$get_storage_account(storage_account_id)

  sas_token <- get_account_sas(storage_account, permissions = "rw")

  return(sas_token)
}
# nocov end
