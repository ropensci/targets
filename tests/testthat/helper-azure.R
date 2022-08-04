
skip_if_no_azure <- function() {
  skip_if(Sys.getenv("RTARGETS_AZURE_APPID") == "")
  skip_if(Sys.getenv("RTARGETS_AZURE_TENANT") == "")
  skip_if(Sys.getenv("RTARGETS_AZURE_PASSWORD") == "")
  skip_if(Sys.getenv("RTARGETS_AZURE_STORAGE_ACCOUNT") == "")
  skip_if_not_installed("AzureStor")
  skip_if_not_installed("AzureRMR")
  skip_if_offline()
  skip_on_cran()
}

random_container_name <- function() {
  paste0(
    "targets-test-container-",
    substr(
      digest::digest(tempfile(), algo = "sha256"),
      start = 0,
      stop = 5
    )
  )
}
