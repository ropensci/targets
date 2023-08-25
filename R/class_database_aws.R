database_aws_new <- function(
  memory = NULL,
  path = NULL,
  subkey = NULL,
  header = NULL,
  list_columns = NULL,
  list_column_modes = NULL,
  queue = NULL,
  resources = NULL
) {
  database_aws_class$new(
    memory = memory,
    path = path,
    subkey = subkey,
    header = header,
    list_columns = list_columns,
    list_column_modes = list_column_modes,
    queue = queue,
    resources = resources
  )
}

database_aws_class <- R6::R6Class(
  classname = "tar_database_aws",
  inherit = database_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    resources = NULL,
    initialize = function(
      memory = NULL,
      path = NULL,
      subkey = NULL,
      header = NULL,
      list_columns = NULL,
      list_column_modes = NULL,
      queue = NULL,
      resources = NULL
    ) {
      super$initialize(
        memory = memory,
        path = path,
        subkey = subkey,
        header = header,
        list_columns = list_columns,
        list_column_modes = list_column_modes,
        queue = queue
      )
      self$resources <- resources
    },
    validate = function() {
      super$validate()
      tar_assert_inherits(
        self$resources,
        "tar_resources_aws",
        msg = paste(
          "AWS resources must be supplied to the {targets} AWS ",
          "database class. Set resources with tar_option_set()"
        )
      )
      resources_validate(self$resources)
    }
  )
)
