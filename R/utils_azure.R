# Semi-automated tests of Azure Storage integration live in tests/azure/. # nolint
# These tests should not be fully automated because they automatically create
# Azure storage containers (i.e., "buckets") and upload data, which could put an
# unexpected and unfair burden on external contributors from the open source
# community.
# nocov start

# TODO: implement "version" optional argument
azure_head <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  tryCatch(
    AzureStor::storage_endpoint(endpoint, key = auth_key, token = auth_token,
                                sas = auth_sas) |>
      AzureStor::storage_container(name = bucket) |>
      AzureStor::get_storage_properties(key),
    http_400 = function(condition) NULL
  )
}

# TODO: implement "version" optional argument
azure_exists <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  AzureStor::storage_endpoint(endpoint,
                              key = auth_key,
                              token = auth_token,
                              sas = auth_sas) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_file_exists(key)
}

azure_list_etags <- function(
  prefix = NULL,
  bucket,
  endpoint,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  if (stringr::str_trim(prefix) == "") {
    prefix <- NULL
  }
  container <- AzureStor::storage_endpoint(endpoint,
                                           key = auth_key,
                                           token = auth_token,
                                           sas = auth_sas) |>
    AzureStor::storage_container(name = bucket)
  files <- container |>
    AzureStor::list_storage_files() |>
    tibble::as_tibble()
  if (nrow(files) > 0) {
    files <- dplyr::filter(files, !isdir)
    if (!is.null(prefix)) {
      files <- files |>
        dplyr::filter(stringr::str_starts(fs::path_file(name), prefix))
    }
    if (nrow(files) > 0) {
      files$name |>
        purrr::map(\(x) AzureStor::get_storage_properties(container, x)$etag) |>
        setNames(files$name)
    } else {
      list()
    }
  } else {
    list()
  }
}

# TODO: implement "version" optional argument
azure_download <- function(
  file,
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  fs::path_dir(file) |>
    fs::dir_create()
  AzureStor::storage_endpoint(endpoint,
                              key = auth_key,
                              token = auth_token,
                              sas = auth_sas) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_download(src = key, dest = file, overwrite = TRUE)
}

# TODO: implement "version" optional argument
azure_delete <- function(
  key,
  bucket,
  endpoint,
  version = NULL,
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  AzureStor::storage_endpoint(endpoint,
                              key = auth_key,
                              token = auth_token,
                              sas = auth_sas) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::delete_storage_file(file = key, confirm = FALSE)
  invisible()
}

# TODO: implement "metadata" optional argument
azure_upload <- function(
  file,
  key,
  bucket,
  endpoint,
  metadata = list(),
  auth_key = NULL,
  auth_token = azure_auth_token(),
  auth_sas = NULL
) {
  # meta <- NULL
  # if (length(metadata) > 0) {
  #   meta <- AzureStor::storage_endpoint(endpoint,
  #                                       key = auth_key,
  #                                       token = auth_token,
  #                                       sas = auth_sas) |>
  #     AzureStor::storage_container(name = bucket) |>
  #     AzureStor::get_storage_metadata(file)
  # }
  AzureStor::storage_endpoint(endpoint,
                              key = auth_key,
                              token = auth_token,
                              sas = auth_sas) |>
    AzureStor::storage_container(name = bucket) |>
    AzureStor::storage_upload(src = file, dest = key)
  # # the following will not change file metadata
  # # per ?AzureStor::set_storage_metadata, metadata must be supplied as
  # # name-value pairs (e.g., `name1 = "value1"`) in order to properly set
  # # metadata for blob or file
  # # TODO: determine how to convert meta object to name-value pair
  # if (!is.null(meta)) {
  #   AzureStor::storage_endpoint(endpoint,
  #                               key = auth_key,
  #                               token = auth_token,
  #                               sas = auth_sas) |>
  #     AzureStor::storage_container(name = bucket) |>
  #     AzureStor::set_storage_metadata(file, meta)
  # }
}

azure_auth_token <- function(
  resource = "https://storage.azure.com",
  tenant = "common",
  app = NULL,
  auth_type = "device_code"
) {
  tryCatch(
    AzureAuth::get_managed_token(resource),
    error = function(err) {
      if (is.null(app)) {
        app <- try(AzureRMR::get_azure_login()$token$client$client_id,
                   silent = TRUE)
        if (inherits(app, "try-error")) {
          AzureRMR::create_azure_login()
          app <- AzureRMR::get_azure_login()$token$client$client_id
        }
      }
      tokens <- AzureRMR::list_azure_tokens()
      resources <- purrr::map(tokens, \(x) x$resource)
      token_use <- match(resource, resources)[1]
      if (!is.na(token_use)) {
        tokens[[token_use]]
      } else {
        AzureRMR::get_azure_token(resource,
                                  tenant = tenant,
                                  app = app,
                                  auth_type = auth_type)
      }
    }
  )
}
# nocov end
