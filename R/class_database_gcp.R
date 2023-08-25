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
        self$resources,
        "tar_resources_gcp",
        msg = paste(
          "gcp resources must be supplied to the {targets} gcp ",
          "database class. Set resources with tar_option_set()"
        )
      )
      resources_validate(self$resources)
    },
    download = function() {
      gcp <- self$resources$gcp
      network <- self$resources$network
      file <- file_init(path = path)
      file_ensure_hash(file)
      gcp_gcs_download(
        file = self$path,
        key = self$key,
        bucket = gcp$bucket,
        seconds_interval = network$seconds_interval %|||% 1,
        seconds_timeout = network$seconds_timeout %|||% 60,
        max_tries = network$max_tries %|||% 5L,
        verbose = network$verbose %|||% TRUE
      )
      invisible()
    },
    upload = function() {
      gcp <- self$resources$gcp
      network <- self$resources$network
      gcp_gcs_upload(
        file = self$path,
        key = self$key,
        bucket = gcp$bucket,
        predefined_acl = gcp$predefined_acl %|||% "private",
        seconds_interval = network$seconds_interval %|||% 1,
        seconds_timeout = network$seconds_timeout %|||% 60,
        max_tries = network$max_tries %|||% 5L,
        verbose = network$verbose %|||% TRUE
      )
      invisible()
    },
  )
)
