# Covered in semi-automated cloud tests.
# nocov start
database_gcp_new <- function(
  memory = NULL,
  path = NULL,
  key = NULL,
  header = NULL,
  list_columns = NULL,
  list_column_modes = NULL,
  queue = NULL,
  resources = NULL
) {
  database_gcp_class$new(
    memory = memory,
    path = path,
    key = key,
    header = header,
    list_columns = list_columns,
    list_column_modes = list_column_modes,
    queue = queue,
    resources = resources
  )
}

database_gcp_class <- R6::R6Class(
  classname = "tar_database_gcp",
  inherit = database_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    validate = function() {
      super$validate()
      tar_assert_inherits(
        self$resources$gcp,
        "tar_resources_gcp",
        msg = paste(
          "GCP resources must be supplied to the {targets} GCP ",
          "database class. Set resources with tar_option_set()"
        )
      )
      resources_validate(self$resources$gcp)
    },
    download = function(verbose = TRUE) {
      if (verbose) {
        tar_print(
          "Downloading GCP cloud object ",
          self$key,
          " to local file ",
          self$path
        )
      }
      gcp <- self$resources$gcp
      dir_create(dirname(self$path))
      gcp_gcs_download(
        file = self$path,
        key = self$key,
        bucket = gcp$bucket,
        max_tries = gcp$max_tries %|||% 5L,
        verbose = gcp$verbose %|||% TRUE
      )
      invisible()
    },
    upload = function(verbose = TRUE) {
      if (verbose) {
        tar_print(
          "Uploading local file ",
          self$path,
          " to GCP cloud object ",
          self$key
        )
      }
      gcp <- self$resources$gcp
      file <- file_init(path = path)
      file_ensure_hash(file)
      gcp_gcs_upload(
        file = self$path,
        key = self$key,
        bucket = gcp$bucket,
        metadata = list(
          "targets-database-hash" = file$hash,
          "targets-database-size" = file$size,
          "targets-database-time" = file$time
        ),
        predefined_acl = gcp$predefined_acl %|||% "private",
        max_tries = gcp$max_tries %|||% 5L,
        verbose = gcp$verbose %|||% TRUE
      )
      invisible()
    },
    head = function() {
      gcp <- self$resources$gcp
      head <- gcp_gcs_head(
        key = self$key,
        bucket = gcp$bucket,
        max_tries = gcp$max_tries %|||% 5L,
        verbose = gcp$verbose %|||% TRUE
      )
      list(
        exists = !is.null(head),
        hash = head$metadata$`targets-database-hash`,
        size = head$metadata$`targets-database-size`,
        time = head$metadata$`targets-database-time`
      )
    },
    delete_cloud = function(verbose = TRUE) {
      if (verbose) {
        tar_print("Deleting GCP cloud object ", self$key)
      }
      gcp <- self$resources$gcp
      head <- gcp_gcs_delete(
        key = self$key,
        bucket = gcp$bucket,
        max_tries = gcp$max_tries %|||% 5L,
        verbose = gcp$verbose %|||% TRUE
      )
    }
  )
)
# nocov end
