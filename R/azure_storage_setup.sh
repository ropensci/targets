#!/bin/bash

default_storage_account="rtargetstesting"


### Default Azure subscription
default_subscription_id="$(az account list --query "[?isDefault].id" -o tsv)"
echo "AZURE_SUBSCRIPTONID=\"$default_subscription_id\"" >> ~/.Renviron

## Create a resource group if it does not exist
echo "RTARGETS_AZURE_RESOURCEGROUP=\"rtargets\"" >> ~/.Renviron
if [ $(az group exists --name $default_resource_group) = false ]; then
    az group create --name ${default_resource_group} \
        --location ${default_location}
fi

### Create service principal with password
# Creation is necessary, otherwise there is no automated method
#  for obtaining the password
creation_response=$(az ad sp create-for-rbac --name "rtargets")
app_id=$(echo $creation_response | jq '.appId' | sed /\"//g)

### Set credentials as environment variables
echo "RTARGETS_AZURE_APPID=\"$app_id\"" >> ~/.Renviron
echo "RTARGETS_AZURE_TENANT=$(echo $creation_response | jq '.tenant')" >> ~/.Renviron
echo "RTARGETS_AZURE_PASSWORD=$(echo $creation_response | jq '.password')" >> ~/.Renviron

### Add roles for blob and storage account management
az role assignment create --assignee $app_id \
    --role "Storage Blob Data Owner" \
    --resource-group $default_resource_group

az role assignment create --assignee $app_id \
    --role "Storage Account Contributor" \
    --resource-group $default_resource_group

### Create storage account
# SKU: the type of storage account. See https://docs.microsoft.com/en-us/rest/api/storagerp/srp_sku_types
# kind: https://docs.microsoft.com/en-us/azure/storage/common/storage-account-overview
az storage account create \
  --name ${default_storage_account} \
  --resource-group ${default_resource_group} \
  --location ${default_location} \
  --sku Standard_RAGRS \
  --kind StorageV2

### Enable blob versioning for storage account
az storage account blob-service-properties update \
    --resource-group $default_resource_group \
    --account-name $default_storage_account \
    --enable-versioning true

echo "RTARGETS_AZURE_STORAGE_ACCOUNT=\"$default_storage_account\"" >> ~/.Renviron
