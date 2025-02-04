# Covered in semi-automated cloud tests.
# nocov start
database_gcp_new <- function(
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
  database_gcp_class$new(
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
          "GCP resources must be supplied to the `targets` GCP ",
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
    download_workspace = function(name, store, verbose = TRUE) {
      path <- path_workspace(store, name)
      key <- path_workspace(dirname(dirname(self$key)), name)
      gcp <- self$resources$gcp
      if (verbose) {
        tar_print(
          "Downloading gcp workspace file ",
          key,
          " to local file ",
          path
        )
      }
      dir_create(dirname(path))
      gcp_gcs_download(
        file = path,
        key = key,
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
    upload_workspace = function(target, meta, reporter) {
      name <- target_get_name(target)
      path <- path_workspace(meta$store, name)
      key <- path_workspace(dirname(dirname(self$key)), name)
      gcp <- self$resources$gcp
      gcp_gcs_upload(
        file = path,
        key = key,
        bucket = gcp$bucket,
        predefined_acl = gcp$predefined_acl %|||% "private",
        max_tries = gcp$max_tries %|||% 5L,
        verbose = gcp$verbose %|||% TRUE
      )
      reporter$report_workspace_upload(target)
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
    },
    delete_cloud_workspaces = function() {
      prefix <- dirname(path_workspace(dirname(dirname(self$key)), "x"))
      gcp <- self$resources$gcp
      keys <- names(
        gcp_gcs_list_md5s(
          prefix = prefix,
          bucket = gcp$bucket,
          verbose = FALSE,
          max_tries = gcp$max_tries %|||% 5L
        )
      )
      for (key in keys) {
        gcp_gcs_delete(
          key = key,
          bucket = gcp$bucket,
          verbose = FALSE,
          max_tries = gcp$max_tries %|||% 5L
        )
      }
      invisible()
    }
  )
)
# nocov end
