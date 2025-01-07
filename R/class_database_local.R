database_local_new <- function(
  lookup = NULL,
  path = NULL,
  key = NULL,
  header = NULL,
  logical_columns = NULL,
  integer_columns = NULL,
  numeric_columns = NULL,
  list_columns = NULL,
  list_column_modes = NULL,
  resources = NULL
) {
  database_local_class$new(
    lookup = lookup,
    path = path,
    key = key,
    header = header,
    logical_columns = logical_columns,
    integer_columns = integer_columns,
    numeric_columns = numeric_columns,
    list_columns = list_columns,
    list_column_modes = list_column_modes,
    resources = resources
  )
}

database_local_class <- R6::R6Class(
  classname = "tar_database_local",
  inherit = database_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    upload = function(verbose = TRUE) {
      if (verbose) {
        tar_print(self$path, " not configured to upload to the cloud.")
      }
      invisible()
    },
    download = function(verbose = TRUE) {
      if (verbose) {
        tar_print(self$path, " not configured to download from the cloud.")
      }
      invisible()
    },
    delete_cloud = function(verbose = TRUE) {
      if (verbose) {
        tar_print("Not configured to delete cloud object ", self$key)
      }
      invisible()
    }
  )
)
